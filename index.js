const assert = require('assert');

const TokenType = {
  LeftBracket: 'LeftBracket',
  RightBracket: 'RightBracket',
  Symbol: 'Symbol',
  Atom: 'Atom',
  Number: 'Number',
};

class Token {
  constructor(tokenType, start, lexeme, literal) {
    this.tokenType = tokenType;
    this.start = start;
    this.lexeme = lexeme;
    this.literal = literal;
  }
}

class Scanner {
  start = 0;
  current = 0;
  tokens = [];

  constructor(source) {
    this.source = source;
  }

  scan() {
    while (!this.isAtEnd()) {
      this.start = this.current;
      const char = this.advance();

      switch (char) {
        case '(':
          this.addToken(TokenType.LeftBracket);
          break;
        case ')':
          this.addToken(TokenType.RightBracket);
          break;
        case ' ':
        case '\n':
          break;
        default:
          if (this.isDigit(char)) {
            while (this.isDigitOrDot(this.peek())) {
              this.advance();
            }
            const numStr = this.source.slice(this.start, this.current);
            const num = parseFloat(numStr);
            this.addTokenWithLiteral(TokenType.Number, num);
            break;
          } else if (this.isIdentifierChar(char)) {
            while (this.isIdentifierChar(this.peek())) {
              this.advance();
            }
            this.addToken(TokenType.Symbol);
            break;
          }
          throw new Error('Unknown token ' + char);
      }
    }
    return this.tokens;
  }

  isIdentifierChar(char) {
    return (
      this.isDigitOrDot(char) ||
      (char >= 'A' && char <= 'Z') ||
      (char >= 'a' && char <= 'z') ||
      ['+', '-', '.', '*', '/', '<', '=', '>', '!', '?', ':', '$', '%', '_', '&', '~', '^'].includes(char)
    );
  }

  advance() {
    return this.source[this.current++];
  }

  peek() {
    return this.source[this.current];
  }

  isDigit(char) {
    return char >= '0' && char <= '9';
  }

  isDigitOrDot(char) {
    return this.isDigit(char) || char === '.';
  }

  addToken(tokenType) {
    this.addTokenWithLiteral(tokenType);
  }

  addTokenWithLiteral(tokenType, literal) {
    const lexeme = this.source.slice(this.start, this.current);
    const token = new Token(tokenType, this.start, lexeme, literal);
    this.tokens.push(token);
  }

  isAtEnd() {
    return this.current >= this.source.length;
  }
}

class Parser {
  current = 0;

  constructor(tokens) {
    this.tokens = tokens;
  }

  /**
   * Parser grammar:
   *  program    => ( expression )*
   *  expression => "(" ( expression )* ")" | function | atom
   *  function   => "(" "define" SYMBOL expression* ")" | "(" "define" "(" SYMBOL* ")" expression* ")"
   *  atom       => SYMBOL | NUMBER
   */
  parse() {
    const expressions = [];
    while (!this.isAtEnd()) {
      const expr = this.list();
      expressions.push(expr);
    }
    return expressions;
  }

  list() {
    if (this.match(TokenType.LeftBracket)) {
      // if (this.peek().lexeme === 'define') {
      //   return this.define();
      // }

      return this.expression();
    }
    return this.atom();
  }

  // define() {
  //   this.advance(); // define symbol

  //   if (this.match(TokenType.LeftBracket)) {
  //     const name = this.consume(TokenType.Symbol);
  //     const args = [];
  //     while (!this.match(TokenType.RightBracket)) {
  //       args.push(this.advance());
  //     }
  //     const body = this.list();
  //     this.consume(TokenType.RightBracket);
  //     return new DefineExpr(name, new FunctionExpr(args, body));
  //   }

  //   const name = this.consume(TokenType.Symbol);
  //   const value = this.list();
  //   this.consume(TokenType.RightBracket);
  //   return new DefineExpr(name, value);
  // }

  expression() {
    const items = [];
    while (!this.match(TokenType.RightBracket)) {
      items.push(this.list());
    }
    return new ListExpr(items);
  }

  atom() {
    switch (true) {
      case this.match(TokenType.Symbol):
        return new SymbolExpr(this.previous());
      case this.match(TokenType.Number):
        return new LiteralExpr(this.previous().literal);
      default:
        throw new Error('Unable to parse: ' + this.peek().tokenType);
    }
  }

  isAtEnd() {
    return this.current >= this.tokens.length;
  }

  consume(tokenType, message) {
    if (this.check(tokenType)) {
      return this.advance();
    }
    throw new Error(message);
  }

  advance() {
    if (!this.isAtEnd()) {
      this.current++;
    }
    return this.previous();
  }

  match(tokenType) {
    if (this.check(tokenType)) {
      this.current++;
      return true;
    }
    return false;
  }

  check(tokenType) {
    return this.peek().tokenType === tokenType;
  }

  peek() {
    return this.tokens[this.current];
  }

  previous() {
    return this.tokens[this.current - 1];
  }
}

class ListExpr {
  constructor(items) {
    this.items = items;
  }
}

class SymbolExpr {
  constructor(token) {
    this.token = token;
  }
}

class LiteralExpr {
  constructor(value) {
    this.value = value;
  }
}

class DefineExpr {
  constructor(name, value) {
    this.name = name;
    this.value = value;
  }
}

class FunctionExpr {
  constructor(args, body) {
    this.args = args;
    this.body = body;
  }
}

class Interpreter {
  constructor() {
    const env = new Map();
    env.set('*', (args) => args.reduce((a, b) => a * b));
    env.set('+', (args) => args.reduce((a, b) => a + b));
    env.set('-', (args) => args.reduce((a, b) => a - b));
    env.set('/', (args) => args.reduce((a, b) => a / b));
    env.set('define', (args) => {
      const [name, value] = args;
      env.set(name.token.lexeme, this.interpret(value));
    });
    this.env = env;
  }

  interpretAll(expressions) {
    let result;
    for (const expr of expressions) {
      result = this.interpret(expr);
    }
    return result;
  }

  interpret(expr) {
    if (expr instanceof ListExpr) {
      const name = expr.items[0];
      const fn = this.env.get(name.token.lexeme);

      if (name.token.lexeme === 'define') {
        return fn(expr.items.slice(1));
      }

      const args = [];
      for (const arg of expr.items.slice(1)) {
        args.push(this.interpret(arg));
      }
      return fn(args);
    }
    if (expr instanceof LiteralExpr) {
      return expr.value;
    }
    if (expr instanceof SymbolExpr) {
      return this.env.get(expr.token.lexeme);
    }
    // if (expr instanceof DefineExpr) {
    //   this.env.set(expr.name, this.interpret(expr.value));
    //   return;
    // }
    throw new Error('Unknown expression to interpret: ' + expr.constructor.name);
  }
}

class Function {
  constructor(args, declaration) {
    this.declaration = declaration;
    this.args = args;
  }

  call(args) {
    this.declaration(args);
  }
}

const interpreter = new Interpreter();

function run(source) {
  const scanner = new Scanner(source);
  const tokens = scanner.scan();

  const parser = new Parser(tokens);
  const expressions = parser.parse();

  return interpreter.interpretAll(expressions);
}

// Tests from https://github.com/FZSS/scheme/blob/master/tests.scm

assert.equal(run(`10`), '10');
assert.equal(run(`(+ 137 349)`), '486');
assert.equal(run(`(- 1000 334)`), '666');
assert.equal(run(`(* 5 99)`), '495');
assert.equal(run(`(/ 10 5)`), '2');
assert.equal(run(`(+ 2.7 10)`), '12.7');
assert.equal(run(`(+ 21 35 12 7)`), '75');
assert.equal(run(`(* 25 4 12)`), '1200');
assert.equal(run(`(+ (* 3 5) (- 10 6))`), '19');
assert.equal(run(`(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))`), '57');
assert.equal(
  run(`(+ (* 3
  (+ (* 2 4)
     (+ 3 5)))
(+ (- 10 7)
  6))`),
  '57'
);

run('(define size 2)');
assert.equal(run(`size`), '2');
assert.equal(run(`(* 5 size)`), 10);
run('(define pi 3.14159)');
run('(define radius 10)');
assert.equal(run(`(* pi (* radius radius))`), 314.159);
run('(define circumference (* 2 pi radius))');
assert.equal(run(`circumference`), 62.8318);

// run('(define (square x) (* x x))');
