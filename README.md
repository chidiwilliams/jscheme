# jscheme

A Scheme interpreter and REPL written in JavaScript.

## Parser grammar

```text
program     => expression*
expression  => lambda | define | if | set! | let
              | "(" expression* ")" | () | atom
lambda      => "(" "lambda" symbol-list expression* ")"
define      => "(" "define" SYMBOL expression ")"
if          => "(" "if" expression expression expression? ")"
set!        => "(" "set" SYMBOL expression ")"
let         => "(" "let" "(" let-binding* ")" expression* ")"
let-binding => "(" SYMBOL expression ")"
atom        => SYMBOL | NUMBER | TRUE | FALSE | STRING
```
