/**
 * The Scanner scans a source string into a list of tokens
 */
class Scanner {
  start = 0;
  current = 0;
  line = 0;
  /**
   * @type {Token[]}
   */
  tokens = [];

  /**
   *
   * @param {string} source
   */
  constructor(source) {
    this.source = source;
  }

  /**
   * Scans the source string and returns all the found tokens
   */
  scan() {
    try {
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
          case '\r':
          case '\t':
            break;
          case '\n':
            this.line++;
            break;
          case '#':
            if (this.peek() === 't') {
              this.advance();
              this.addToken(TokenType.Boolean, true);
              break;
            }
            if (this.peek() === 'f') {
              this.advance();
              this.addToken(TokenType.Boolean, false);
              break;
            }
          case '"':
            while (this.peek() !== '"' && !this.isAtEnd()) {
              this.advance();
            }
            const literal = this.source.slice(this.start + 1, this.current);
            this.addToken(TokenType.String, literal);
            this.advance();
            break;
          case ';': // Comment - ignore all characters until the end of the line
            while (this.peek() !== '\n' && !this.isAtEnd()) {
              this.advance();
            }
            break;
          default:
            if (this.isDigit(char)) {
              while (this.isDigitOrDot(this.peek())) {
                this.advance();
              }
              const numberAsString = this.source.slice(this.start, this.current);
              const literal = parseFloat(numberAsString);
              this.addToken(TokenType.Number, literal);
              break;
            }
            if (this.isIdentifier(char)) {
              while (this.isIdentifier(this.peek())) {
                this.advance();
              }
              this.addToken(TokenType.Symbol);
              break;
            }
            throw new SyntaxError(this.line, 'Unknown token ' + char);
        }
      }

      this.tokens.push(new Token(TokenType.Eof, 0, '', null, this.line));
      return { tokens: this.tokens, hadError: false };
    } catch (error) {
      if (error instanceof SyntaxError) {
        console.error(error.toString());
        return { tokens: this.tokens, hadError: true };
      }
      throw error;
    }
  }

  /**
   * Returns true if char is is valid symbol character
   *
   * @param {string} char
   */
  isIdentifier(char) {
    return (
      this.isDigitOrDot(char) ||
      (char >= 'A' && char <= 'Z') ||
      (char >= 'a' && char <= 'z') ||
      ['+', '-', '.', '*', '/', '<', '=', '>', '!', '?', ':', '$', '%', '_', '&', '~', '^'].includes(char)
    );
  }

  /**
   * Returns the current character and moves the scanner forward by one
   */
  advance() {
    return this.source[this.current++];
  }

  /**
   * Returns the current character
   */
  peek() {
    return this.source[this.current];
  }

  /**
   * Returns true if char is a digit
   * @param {string} char
   */
  isDigit(char) {
    return char >= '0' && char <= '9';
  }

  /**
   * Returns true if char is a digit or a dot
   * @param {string} char
   */
  isDigitOrDot(char) {
    return this.isDigit(char) || char === '.';
  }

  /**
   * Adds a token with the given type and literal value to the token list
   * @param {TokenType} tokenType
   * @param {any} literal
   */
  addToken(tokenType, literal) {
    const lexeme = this.source.slice(this.start, this.current);
    const token = new Token(tokenType, this.start, lexeme, literal, this.line);
    this.tokens.push(token);
  }

  /**
   * Returns true if scanning is complete
   */
  isAtEnd() {
    return this.current >= this.source.length;
  }
}

/**
 * @enum {string}
 */
const TokenType = {
  LeftBracket: 'LeftBracket',
  RightBracket: 'RightBracket',
  Symbol: 'Symbol',
  Number: 'Number',
  Boolean: 'Boolean',
  String: 'String',
  Eof: 'Eof',
};

/**
 * A Token is a useful unit of the program source
 */
class Token {
  /**
   * @param {TokenType} tokenType
   * @param {number} start
   * @param {string} lexeme
   * @param {any} literal
   * @param {number} line
   */
  constructor(tokenType, start, lexeme, literal, line) {
    this.tokenType = tokenType;
    this.start = start;
    this.lexeme = lexeme;
    this.literal = literal;
    this.line = line;
  }
}

/**
 * A "compile-time" error that occurs during scanning or parsing.
 */
class SyntaxError extends Error {
  /**
   *
   * @param {string} message
   * @param {number} line
   */
  constructor(line, message) {
    super(message);
    this.line = line;
  }

  toString() {
    return `SyntaxError: [line: ${this.line}] ${this.message}`;
  }
}

/**
 * Value of null and the empty list
 */
const NULL_VALUE = [];

/**
 * The Parser parses a list of tokens into an AST
 *
 * Parser grammar:
 * program     => expression*
 * expression  => lambda | define | if | set! | let
 *               | "(" expression* ")" | () | atom
 * define      => "(" "define" SYMBOL expression ")"
 * if          => "(" "if" expression expression expression? ")"
 * set!        => "(" "set" SYMBOL expression ")"
 * let         => "(" "let" "(" let-binding* ")" expression* ")"
 * let-binding => "(" SYMBOL expression ")"
 * atom        => SYMBOL | NUMBER | TRUE | FALSE | STRING
 */
class Parser {
  current = 0;

  /*
   * @param {Token[]} tokens
   */
  constructor(tokens) {
    this.tokens = tokens;
  }

  /**
   * Parses the expressions in the token list
   */
  parse() {
    try {
      const expressions = [];
      while (!this.isAtEnd()) {
        const expr = this.expression();
        expressions.push(expr);
      }
      return expressions;
    } catch (error) {
      if (error instanceof SyntaxError) {
        console.error(error.toString());
        return [];
      }
      throw error;
    }
  }

  /**
   * Parses a list expression
   * @returns {Expr}
   */
  expression() {
    if (this.match(TokenType.LeftBracket)) {
      if (this.match(TokenType.RightBracket)) {
        return new LiteralExpr(NULL_VALUE);
      }

      const token = this.peek();
      if (token.lexeme === 'lambda') {
        return this.lambda();
      }
      if (token.lexeme === 'define') {
        return this.define();
      }
      if (token.lexeme === 'if') {
        return this.if();
      }
      if (token.lexeme === 'set!') {
        return this.set();
      }
      if (token.lexeme === 'let') {
        return this.let();
      }
      return this.call();
    }
    return this.atom();
  }

  /**
   * Parses a call expression
   * @returns {Expr}
   */
  call() {
    const callee = this.expression();
    const args = [];
    while (!this.match(TokenType.RightBracket)) {
      args.push(this.expression());
    }
    return new CallExpr(callee, args);
  }

  /**
   * Parses a lambda expression
   * @returns {Expr}
   */
  lambda() {
    this.advance();

    /**
     * @type {Token[]}
     */
    const params = [];
    this.consume(TokenType.LeftBracket);
    while (!this.match(TokenType.RightBracket)) {
      const token = this.consume(TokenType.Symbol);
      params.push(token);
    }

    /**
     * @type {Expr[]}
     */
    const body = [];
    while (!this.match(TokenType.RightBracket)) {
      body.push(this.expression());
    }

    return new LambdaExpr(params, body);
  }

  /**
   * Parses a define expression
   * @returns {Expr}
   */
  define() {
    this.advance();
    const name = this.consume(TokenType.Symbol);
    const value = this.expression();
    this.consume(TokenType.RightBracket);
    return new DefineExpr(name, value);
  }

  /**
   * Parses an if expression
   * @returns {Expr}
   */
  if() {
    this.advance();
    const condition = this.expression();
    const thenBranch = this.expression();
    let elseBranch;
    if (!this.match(TokenType.RightBracket)) {
      elseBranch = this.expression();
    }
    this.consume(TokenType.RightBracket);
    return new IfExpr(condition, thenBranch, elseBranch);
  }

  /**
   * Parses a set expression
   * @returns {Expr}
   */
  set() {
    this.advance();
    const name = this.consume(TokenType.Symbol);
    const value = this.expression();
    this.consume(TokenType.RightBracket);
    return new SetExpr(name, value);
  }

  /**
   * Parses a let expression
   * @returns {Expr}
   */
  let() {
    this.advance();

    this.consume(TokenType.LeftBracket);

    const bindings = [];
    while (!this.match(TokenType.RightBracket)) {
      bindings.push(this.letBinding());
    }

    /**
     * @type {Expr[]}
     */
    const body = [];
    while (!this.match(TokenType.RightBracket)) {
      body.push(this.expression());
    }

    return new LetExpr(bindings, body);
  }

  letBinding() {
    this.consume(TokenType.LeftBracket);
    const name = this.consume(TokenType.Symbol);
    const value = this.expression();
    this.consume(TokenType.RightBracket);
    return new LetBindingNode(name, value);
  }

  /**
   * Parses an atom expression
   * @returns {Expr}
   */
  atom() {
    switch (true) {
      case this.match(TokenType.Symbol):
        return new SymbolExpr(this.previous());
      case this.match(TokenType.Number):
      case this.match(TokenType.String):
      case this.match(TokenType.Boolean):
        return new LiteralExpr(this.previous().literal);
      default:
        throw new SyntaxError(this.peek().line, `Unexpected token: ${this.peek().tokenType}`);
    }
  }

  /**
   * Returns true if all tokens have been parsed
   */
  isAtEnd() {
    return this.peek().tokenType === TokenType.Eof;
  }

  /**
   * Returns the current token if it matches the given token type. Else, it throws an error.
   * @param {TokenType} tokenType
   */
  consume(tokenType) {
    if (this.check(tokenType)) {
      return this.advance();
    }
    throw new SyntaxError(this.previous().line, `Unexpected token ${this.previous().tokenType}, expected ${tokenType}`);
  }

  /**
   * Returns the current token and advances to the next token
   */
  advance() {
    if (!this.isAtEnd()) {
      this.current++;
    }
    return this.previous();
  }

  /**
   * Returns true if the current token matches the given token type and then advances
   * @param {TokenType} tokenType
   */
  match(tokenType) {
    if (this.check(tokenType)) {
      this.current++;
      return true;
    }
    return false;
  }

  /**
   * Returns true if the current token matches the given token type
   * @param {TokenType} tokenType
   */
  check(tokenType) {
    return this.peek().tokenType === tokenType;
  }

  /**
   * Returns the current token
   */
  peek() {
    return this.tokens[this.current];
  }

  /**
   * Returns the previous token
   */
  previous() {
    return this.tokens[this.current - 1];
  }
}

class Expr {}

class CallExpr extends Expr {
  /**
   * @param {Expr} callee
   * @param {Expr[]} args
   */
  constructor(callee, args) {
    super();
    this.callee = callee;
    this.args = args;
  }
}

class SymbolExpr extends Expr {
  /**
   * @param {Token} token
   */
  constructor(token) {
    super();
    this.token = token;
  }
}

class LiteralExpr extends Expr {
  /**
   * @param {any} value
   */
  constructor(value) {
    super();
    this.value = value;
  }
}

class LambdaExpr extends Expr {
  /**
   * @param {Token[]} params
   * @param {Expr[]} body
   */
  constructor(params, body) {
    super();
    this.params = params;
    this.body = body;
  }
}

class SetExpr extends Expr {
  /**
   * @param {Token} name
   * @param {Expr} value
   */
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}

class DefineExpr extends Expr {
  /**
   * @param {Token} name
   * @param {Expr} value
   */
  constructor(name, value) {
    super();
    this.name = name;
    this.value = value;
  }
}

class IfExpr extends Expr {
  /**
   * @param {Expr} condition
   * @param {Expr} thenBranch
   * @param {Expr} elseBranch
   */
  constructor(condition, thenBranch, elseBranch) {
    super();
    this.condition = condition;
    this.thenBranch = thenBranch;
    this.elseBranch = elseBranch;
  }
}

class LetExpr {
  /**
   * @param {LetBindingNode[]} bindings
   * @param {Expr[]} body
   */
  constructor(bindings, body) {
    this.bindings = bindings;
    this.body = body;
  }
}

class LetBindingNode {
  /**
   * @param {Token} name
   * @param {Expr} value
   */
  constructor(name, value) {
    this.name = name;
    this.value = value;
  }
}

/**
 * The Interpreter interprets (or executes) the parsed expressions
 */
class Interpreter {
  constructor() {
    // Setup the interpreter's environment with the primitive procedures
    const env = new Environment();
    env.define('*', new PrimitiveProcedure((args) => args.reduce((a, b) => a * b, 1)));
    env.define('+', new PrimitiveProcedure((args) => args.reduce((a, b) => a + b, 0)));
    env.define('-', new PrimitiveProcedure((args) => args.reduce((a, b) => a - b)));
    env.define('/', new PrimitiveProcedure((args) => args.reduce((a, b) => a / b)));
    env.define('=', new PrimitiveProcedure((args) => args.reduce((a, b) => a === b)));
    env.define('<=', new PrimitiveProcedure((args) => args.reduce((a, b) => a <= b)));
    env.define('>=', new PrimitiveProcedure((args) => args.reduce((a, b) => a >= b)));
    env.define('string-length', new PrimitiveProcedure(([str]) => str.length));
    env.define('string-append', new PrimitiveProcedure((args) => args.reduce((a, b) => a + b)));
    env.define('list', new PrimitiveProcedure((args) => args));
    env.define('null?', new PrimitiveProcedure(([arg]) => arg === NULL_VALUE));
    env.define('list?', new PrimitiveProcedure(([arg]) => arg instanceof Array));
    env.define('number?', new PrimitiveProcedure(([arg]) => arg instanceof Number));
    env.define(
      'procedure?',
      new PrimitiveProcedure(([arg]) => arg instanceof Procedure || arg instanceof PrimitiveProcedure)
    );
    env.define('car', new PrimitiveProcedure(([arg]) => arg[0]));
    env.define('cdr', new PrimitiveProcedure(([arg]) => (arg.length > 1 ? arg.slice(1) : NULL_VALUE)));
    env.define('cons', new PrimitiveProcedure(([a, b]) => [a, ...b]));
    env.define('remainder', new PrimitiveProcedure(([a, b]) => a % b));
    env.define('quote', new PrimitiveProcedure(([arg]) => arg));
    env.define('begin', new PrimitiveProcedure((args) => args[args.length - 1]));
    env.define('equal?', new PrimitiveProcedure(([a, b]) => a === b));
    env.define('not', new PrimitiveProcedure(([arg]) => !arg));
    env.define('round', new PrimitiveProcedure(([arg]) => Math.round(arg)));
    env.define('abs', new PrimitiveProcedure(([arg]) => Math.abs(arg)));
    env.define('display', new PrimitiveProcedure(([arg]) => console.log(stringify(arg))));
    env.define('apply', new PrimitiveProcedure(([proc, args]) => proc.call(this, args)));
    this.env = env;
  }

  /**
   * Executes the list of expressions and returns the result. If a
   * RuntimeError occurs, it prints the error and returns the null value.
   * @param {Expr[]} expressions
   */
  interpretAll(expressions) {
    try {
      let result;
      for (const expr of expressions) {
        result = this.interpret(expr, this.env);
      }
      return result;
    } catch (error) {
      if (error instanceof RuntimeError) {
        console.error(error.toString());
        return NULL_VALUE;
      }
      throw error;
    }
  }

  /**
   * Executes an expression within an environment and returns its value
   * @param {Expr} expr
   * @param {Environment} env
   */
  interpret(expr, env) {
    while (true) {
      if (expr instanceof CallExpr) {
        const callee = this.interpret(expr.callee, env);
        const args = expr.args.map((arg) => this.interpret(arg, env));

        // Eliminate the tail-call of running this procedure by "continuing the interpret loop"
        // with the procedure's body set as the current expression and the procedure's closure set
        // as the current environment
        if (callee instanceof Procedure) {
          const callEnv = new Environment(callee.declaration.params, args, callee.closure);

          for (const exprInBody of callee.declaration.body.slice(0, -1)) {
            this.interpret(exprInBody, callEnv);
          }
          expr = callee.declaration.body[callee.declaration.body.length - 1];
          env = callEnv;
          continue;
        }
        if (callee instanceof PrimitiveProcedure) {
          return callee.call(this, args);
        }
        throw new RuntimeError('Cannot call ' + stringify(callee));
      }
      if (expr instanceof LiteralExpr) {
        return expr.value;
      }
      if (expr instanceof SymbolExpr) {
        return env.get(expr.token.lexeme);
      }
      if (expr instanceof LambdaExpr) {
        return new Procedure(expr, env);
      }
      if (expr instanceof DefineExpr) {
        env.define(expr.name.lexeme, this.interpret(expr.value, env));
        return;
      }
      if (expr instanceof IfExpr) {
        const condition = this.interpret(expr.condition, env);
        // Eliminate the tail call of the if branches by "continuing the interpret loop"
        // with the branch set as the current expression
        expr = condition !== false ? expr.thenBranch : expr.elseBranch;
        continue;
      }
      if (expr instanceof SetExpr) {
        return env.set(expr.name.lexeme, this.interpret(expr.value, env));
      }
      if (expr instanceof LetExpr) {
        const names = expr.bindings.map((binding) => binding.name);
        const values = expr.bindings.map((binding) => this.interpret(binding.value, env));

        const letEnv = new Environment(names, values, env);

        for (const exprInBody of expr.body.slice(0, -1)) {
          this.interpret(exprInBody, letEnv);
        }

        expr = expr.body[expr.body.length - 1];
        env = letEnv;
        continue;
      }
      throw new Error('Cannot interpret: ' + expr.constructor.name); // Should be un-reachable
    }
  }
}

/**
 * An Environment stores variables and their values
 */
class Environment {
  /**
   *
   * @param {Token[]=} names
   * @param {any[]=} values
   * @param {Environment=} enclosing
   */
  constructor(names = [], values, enclosing) {
    /**
     * @type {Map<string, any>}
     */
    this.values = new Map();
    names.forEach((param, i) => {
      this.values.set(param.lexeme, values[i]);
    });

    this.enclosing = enclosing;
  }

  /**
   * Define the value of name in the current environment
   * @param {string} name
   * @param {any} value
   */
  define(name, value) {
    this.values.set(name, value);
  }

  /**
   * Sets the value of name defined in this environment or an enclosing environment
   * @param {string} name
   * @param {any} value
   */
  set(name, value) {
    if (this.values.has(name)) {
      return this.values.set(name, value);
    }
    if (this.enclosing) {
      return this.enclosing.set(name, value);
    }
    throw new RuntimeError('Unknown identifier: ' + name);
  }

  /**
   * Gets the value of name from the environment. If the name doesn't
   * exist in this environment, it checks in the enclosing environment.
   * When there is no longer an enclosing environment to check and the
   * name is not found, it throws a RuntimeError.
   * @param {string} name
   */
  get(name) {
    if (this.values.has(name)) {
      return this.values.get(name);
    }
    if (this.enclosing) {
      return this.enclosing.get(name);
    }
    throw new RuntimeError('Unknown identifier: ' + name);
  }
}

/**
 * A primitive procedure that can be called from the program.
 */
class PrimitiveProcedure {
  /**
   *
   * @param {Function} declaration
   */
  constructor(declaration) {
    this.declaration = declaration;
  }

  /**
   *
   * @param {Interpreter} _
   * @param {any[]} params
   */
  call(_, params) {
    return this.declaration(params);
  }
}

/**
 * A Procedure defined in the program
 */
class Procedure {
  /**
   * @param {LambdaExpr} declaration
   * @param {Environment} closure
   */
  constructor(declaration, closure) {
    this.declaration = declaration;
    this.closure = closure;
  }

  /**
   * Sets up the environment for the procedure given its
   * arguments and then runs the procedure's body
   * @param {Interpreter} interpreter
   * @param {any[]} args
   * @returns
   */
  call(interpreter, args) {
    const env = new Environment(this.declaration.params, args, this.closure);
    let result;
    for (const expr of this.declaration.body) {
      result = interpreter.interpret(expr, env);
    }
    return result;
  }
}

/**
 * A run-time error
 */
class RuntimeError extends Error {
  constructor(message) {
    super(message);
  }

  toString() {
    return 'Error: ' + this.message;
  }
}

let interpreter = new Interpreter();

function run(source) {
  const scanner = new Scanner(source);
  const { tokens, hadError } = scanner.scan();
  if (hadError) {
    return;
  }

  const parser = new Parser(tokens);
  const expressions = parser.parse();

  const result = interpreter.interpretAll(expressions);
  return stringify(result);
}

/**
 * Converts the given value to a printable string
 * @param {any} value
 * @returns {string}
 */
function stringify(value) {
  if (value === false) return '#f';
  if (value === true) return '#t';
  if (Array.isArray(value)) return '(' + value.map(stringify).join(' ') + ')';
  if (value instanceof PrimitiveProcedure) return 'PrimitiveProcedure';
  if (value instanceof Procedure) return 'Procedure';
  if (typeof value === 'string') return `"${value}"`;
  if (typeof value === 'number') return `${value}`;
  return String(value);
}

// TESTS
// Source: https://github.com/FZSS/scheme/blob/master/tests.scm

const assert = require('assert');

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
assert.equal(run(`(define str "hello world") str`), '"hello world"');
assert.equal(run(`(string-length str)`), '11');
assert.equal(run(`(string-append str " here")`), '"hello world here"');

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

run(`(define sum-to
  (lambda (n acc)
    (if (= n 0)
        acc
        (sum-to (- n 1) (+ n acc)))))`);
assert.equal(run(`(sum-to 10000 0)`), '50005000'); // TCE!

run(`)(define a "scheme_read test")`); // Unexpected token: RightBracket

run(`(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))`);
assert.equal(run(`(range 0 10)`), '(0 1 2 3 4 5 6 7 8 9)');

run(`(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))`);
assert.equal(run(`(fact 100)`), '9.33262154439441e+157');

assert.equal(run(`(begin (define r 10) (* pi (* r r)))`), '314.159');

assert.equal(run(`(apply + (list 10 20 30))`), '60');

assert.equal(run(`(apply map (list fact (list 5 10)))`), '(120 3628800)');

run(`§§`); // SyntaxError: Unknown token

run(`(define x 3)
(define foo (lambda () (define x 4) x))
(define bar (lambda () (set! x 4) x))
  `);

assert.equal(run(`(foo)`), '4');
assert.equal(run(`x`), '3');
assert.equal(run(`(bar)`), '4');
assert.equal(run(`x`), '4');

run(`(define f (lambda (x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b)))))
`);
assert.equal(run(`(f 3 4)`), '456');

run(`
(`); // Unexpected Eof

run(`(define gen (lambda ()
(let ((n 0))
  (lambda ()
    (set! n (+ n 1))
    n))))`);

run(`(define next (gen))`);
run(`(display (next))`);
run(`(display (next))`);
run(`(display (next))`);

assert.equal(run(`(* 2 (* 8 4)) ; end of line comment`), 64);

console.log('tests successful...\n');

// REPL

interpreter = new Interpreter();

const repl = require('repl');
repl.start({
  prompt: 'jscheme> ',
  eval: (input, context, filename, callback) => {
    callback(null, run(input));
  },
});
