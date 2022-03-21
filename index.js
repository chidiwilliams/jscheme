const TokenType = {
  LeftBracket: 'LeftBracket',
  RightBracket: 'RightBracket',
  Symbol: 'Symbol',
  Number: 'Number',
  True: 'True',
  False: 'False',
  String: 'String',
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
        case '#':
          if (this.peek() === 't') {
            this.advance();
            this.addToken(TokenType.True);
            break;
          }
          if (this.peek() === 'f') {
            this.advance();
            this.addToken(TokenType.False);
            break;
          }
        case '"':
          while (this.peek() !== '"' && !this.isAtEnd()) {
            this.advance();
          }
          const str = this.source.slice(this.start + 1, this.current);
          this.addTokenWithLiteral(TokenType.String, str);
          this.advance();
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
   *  program    => ( list )*
   *  list       => lambda | define | if | "(" ( list )* ")" | function | atom
   *  lambda     => "(" "lambda" list list ")"
   *  define     => "(" "define" IDENTIFIER list ")"
   *  if         => "(" "if" list list list? ")"
   *  atom       => SYMBOL | NUMBER | TRUE | FALSE | STRING
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
      if (this.peek().lexeme === 'lambda') {
        return this.lambda();
      }
      if (this.peek().lexeme === 'define') {
        return this.define();
      }
      if (this.peek().lexeme === 'if') {
        return this.if();
      }

      const items = [];
      while (!this.match(TokenType.RightBracket)) {
        items.push(this.list());
      }
      return new ListExpr(items);
    }
    return this.atom();
  }

  lambda() {
    this.advance();
    const args = this.list();
    const body = this.list();
    this.consume(TokenType.RightBracket);
    return new LambdaExpr(args, body);
  }

  define() {
    this.advance();
    const name = this.consume(TokenType.Symbol);
    const value = this.list();
    this.consume(TokenType.RightBracket);
    return new DefineExpr(name, value);
  }

  if() {
    this.advance();
    const cond = this.list();
    const thenBranch = this.list();
    let elseBranch;
    if (!this.match(TokenType.RightBracket)) {
      elseBranch = this.list();
    }
    this.consume(TokenType.RightBracket);
    return new IfExpr(cond, thenBranch, elseBranch);
  }

  atom() {
    switch (true) {
      case this.match(TokenType.Symbol):
        return new SymbolExpr(this.previous());
      case this.match(TokenType.Number):
        return new LiteralExpr(this.previous().literal);
      case this.match(TokenType.True):
        return new LiteralExpr(true);
      case this.match(TokenType.False):
        return new LiteralExpr(false);
      case this.match(TokenType.String):
        return new LiteralExpr(this.previous().literal);
      default:
        throw new Error('Unable to parse: ' + this.peek().tokenType);
    }
  }

  isAtEnd() {
    return this.current >= this.tokens.length;
  }

  consume(tokenType) {
    if (this.check(tokenType)) {
      return this.advance();
    }
    throw new Error('Expected ' + tokenType);
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

class LambdaExpr {
  constructor(args, body) {
    this.args = args;
    this.body = body;
  }
}

class DefineExpr {
  constructor(name, value) {
    this.name = name;
    this.value = value;
  }
}

class IfExpr {
  constructor(condition, thenBranch, elseBranch) {
    this.condition = condition;
    this.thenBranch = thenBranch;
    this.elseBranch = elseBranch;
  }
}

class Environment {
  constructor(enclosing) {
    this.values = new Map();
    this.enclosing = enclosing;
  }

  set(name, value) {
    this.values.set(name, value);
  }

  get(name) {
    if (this.values.has(name)) {
      return this.values.get(name);
    }
    if (this.enclosing) {
      return this.enclosing.get(name);
    }
    throw new Error('Unknown identifier: ' + name);
  }
}

class Interpreter {
  constructor() {
    const env = new Environment();
    env.set('*', { call: (args) => args.reduce((a, b) => a * b) });
    env.set('+', { call: (args) => args.reduce((a, b) => a + b) });
    env.set('-', { call: (args) => args.reduce((a, b) => a - b) });
    env.set('/', { call: (args) => args.reduce((a, b) => a / b) });
    env.set('=', { call: (args) => args.reduce((a, b) => a === b) });
    env.set('<=', { call: (args) => args.reduce((a, b) => a <= b) });
    env.set('>=', { call: (args) => args.reduce((a, b) => a >= b) });
    env.set('string-length', { call: (args) => args[0].length });
    env.set('string-append', { call: (args) => args[0] + args[1] });
    env.set('list', { call: (args) => args });
    env.set('null?', { call: (args) => Array.isArray(args[0]) && args[0].length === 0 });
    env.set('car', { call: (args) => args[0][0] });
    env.set('cdr', { call: (args) => args[0].slice(1) });
    env.set('cons', { call: (args) => [args[0], ...args[1]] });
    env.set('remainder', { call: (args) => args[0] % args[1] });
    this.env = env;
  }

  interpretAll(expressions, env) {
    let result;
    for (const expr of expressions) {
      result = this.interpret(expr, env);
    }
    return this.stringify(result);
  }

  stringify(expr) {
    if (expr === false) {
      return '#f';
    }
    if (expr === true) {
      return '#t';
    }
    if (expr === undefined) {
      return '#f';
    }
    if (Array.isArray(expr)) {
      return '(' + expr.join(' ') + ')';
    }
    return expr.toString();
  }

  interpret(expr, env) {
    while (true) {
      if (expr instanceof ListExpr) {
        const [name, ...body] = expr.items;

        const args = [];
        for (const arg of body) {
          args.push(this.interpret(arg, env));
        }

        const callee = this.interpret(name, env);
        return callee.call(args, env);
      }
      if (expr instanceof LiteralExpr) {
        return expr.value;
      }
      if (expr instanceof SymbolExpr) {
        return env.get(expr.token.lexeme);
      }
      if (expr instanceof LambdaExpr) {
        return new Function(expr.args, expr.body);
      }
      if (expr instanceof DefineExpr) {
        return env.set(expr.name.lexeme, this.interpret(expr.value, env));
      }
      if (expr instanceof IfExpr) {
        const cond = this.interpret(expr.condition, env);
        if (cond !== false) {
          expr = expr.thenBranch;
          continue;
        }
        expr = expr.elseBranch;
        continue;
      }
      throw new Error('Unknown expression to interpret: ' + expr.constructor.name);
    }
  }
}

class Function {
  constructor(args, declaration) {
    this.declaration = declaration;
    this.args = args;
  }

  call(args, env) {
    const fnEnv = new Environment(env);
    for (let i = 0; i < this.args.items.length; i++) {
      const arg = this.args.items[i];
      fnEnv.set(arg.token.lexeme, args[i]);
    }
    return interpreter.interpret(this.declaration, fnEnv);
  }
}

const interpreter = new Interpreter();

const env = new Environment();
env.set('*', { call: (args) => args.reduce((a, b) => a * b) });
env.set('+', { call: (args) => args.reduce((a, b) => a + b) });
env.set('-', { call: (args) => args.reduce((a, b) => a - b) });
env.set('/', { call: (args) => args.reduce((a, b) => a / b) });
env.set('=', { call: (args) => args.reduce((a, b) => a === b) });
env.set('<=', { call: (args) => args.reduce((a, b) => a <= b) });
env.set('>=', { call: (args) => args.reduce((a, b) => a >= b) });
env.set('string-length', { call: (args) => args[0].length });
env.set('string-append', { call: (args) => args[0] + args[1] });
env.set('list', { call: (args) => args });
env.set('null?', { call: (args) => Array.isArray(args[0]) && args[0].length === 0 });
env.set('car', { call: (args) => args[0][0] });
env.set('cdr', { call: (args) => args[0].slice(1) });
env.set('cons', { call: (args) => [args[0], ...args[1]] });
env.set('remainder', { call: (args) => args[0] % args[1] });

function run(source) {
  const scanner = new Scanner(source);
  const tokens = scanner.scan();

  const parser = new Parser(tokens);
  const expressions = parser.parse();

  return interpreter.interpretAll(expressions, env);
}

const assert = require('assert');

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

run('(define square (lambda (x) (* x x)))');
assert.equal(run(`(square 21)`), '441');

run('(define and (lambda (x y) (if x y #f)))');

run('(define x 3)');
run('(define y 4)');
assert.equal(run(`(and (if (= x 3) #t #f) (if (= y 4) #t #f))`), '#t');

run('(define or (lambda (x y) (if x x y)))');
assert.equal(run(`(or #f #t)`), '#t');

run(`(define fibonacci
  (lambda (num)
    (if (<= num 1)
        num
        (+ (fibonacci (- num 1)) (fibonacci (- num 2))))))`);
assert.equal(run(`(fibonacci 20)`), '6765');

assert.equal(run(`((lambda (x) (* x 2)) 7)`), 14);
assert.equal(run(`(define str "hello world") str`), 'hello world');
assert.equal(run(`(string-length str)`), '11');
assert.equal(run(`(string-append str " here")`), 'hello world here');

assert.equal(run(`(define one-through-four (list 1 2 3 4)) one-through-four`), '(1 2 3 4)');
assert.equal(run(`(car one-through-four)`), '1');
assert.equal(run(`(car (cdr one-through-four))`), '2');
assert.equal(run(`(null? (cdr (list)))`), '#t');
run(`(define map
  (lambda (proc items)
    (if (null? items)
        (list)
        (cons (proc (car items))
              (map proc (cdr items))))))`);
assert.equal(run(`(map square (list 1 2 3))`), '(1 4 9)');
run(`(define odd? (lambda (x) (= 1 (remainder x 2))))`);
run(`(define filter
  (lambda (predicate sequence)
    (if (null? sequence)
        (list)
        (if (predicate (car sequence))
            (cons (car sequence)
                  (filter predicate (cdr sequence)))
            (filter predicate (cdr sequence))))))`);
assert.equal(run(`(filter odd? (list 1 2 3 4 5))`), '(1 3 5)');
