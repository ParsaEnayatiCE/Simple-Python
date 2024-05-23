#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "lexer.rkt")
(require "datatypes.rkt")

(define full-parser
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (program ((statements) (a-program $1)))
    (statements
     ((statement SEMICOLON) (a-statement $1))
     ((statements statement SEMICOLON) (some-statements $1 $2)))
    (statement
     ((comp-stmt) (compound-statement $1))
     ((smpl-stmt) (simple-statement $1)))
    (smpl-stmt
     ((assignment) (assignment-statement $1))
     ((glbl-stmt) (global-statement $1))
     ((rtrn-stmt) (return-statement $1))
     ((PASS) (pass-statement))
     ((BREAK) (break-statement))
     ((CONTINUE) (continue-statement)))
    (comp-stmt
     ((func-def) (function-definition $1))
     ((if-stmt) (if-statement $1))
     ((for-stmt) (for-statement $1)))
    (assignment ((ID ASSIGNMENT expression) (an-assignment (id-exp $1) $3)))
    (rtrn-stmt
     ((RETURN) (void-return))
     ((RETURN expression) (exp-return $2)))
    (glbl-stmt ((GLOBAL ID) (a-global (id-exp $2))))
    (func-def
     ((DEF ID OP CP COLON statements) (function-statement-noargs (id-exp $2) $6))
     ((DEF ID OP parameters CP COLON statements) (function-statement (id-exp $2) $4 $7)))
    (parameters
     ((assignment) (assignment-parameter $1))
     ((parameters COMMA assignment) (all-parameters $1 $3)))
    (if-stmt ((IF expression COLON statements else-stmt) (an-if-exp $2 $4 $5)))
    (else-stmt ((ELSE COLON statements) (an-else-exp $3)))
    (for-stmt ((FOR ID IN expression COLON statements) (a-for-exp (id-exp $2) $4 $6)))
    (expression
     ((disjunction) (an-expression $1)))
    (disjunction
     ((conjunction) (conjunction-exp $1))
     ((disjunction OR conjunction) (a-disjunction $1 $3)))
    (conjunction
     ((inversion) (inversion-exp $1))
     ((conjunction AND inversion) (a-conjunction $1 $3)))
    (inversion
     ((NOT inversion) (an-inversion $2))
     ((comparison) (comp-exp $1)))
    (comparison
     ((eq-sum) (equal-sum $1))
     ((lt-sum) (lessthan-sum $1))
     ((let-sum) (lessthanorequal-sum $1))
     ((gt-sum) (greaterthan-sum $1))
     ((get-sum) (greaterthanorequal-sum $1))
     ((sum) (sum-expression $1)))
    (eq-sum ((sum EQUALS sum) (an-eq-exp $1 $3)))
    (lt-sum ((sum LT sum) (a-lt-exp $1 $3)))
    (let-sum ((sum LET sum) (a-let-exp $1 $3)))
    (gt-sum ((sum GT sum) (a-gt-exp $1 $3)))
    (get-sum ((sum GET sum) (a-get-exp $1 $3)))
    (sum
     ((sum PLUS term) (plus-term $1 $3))
     ((sum MINUS term) (minus-term $1 $3))
     ((term) (term-expression $1)))
    (term
     ((term TIMES factor) (times-factor $1 $3))
     ((term DIVIDES factor) (divides-factor $1 $3))
     ((factor) (factor-expression $1)))
    (factor
     ((PLUS power) (plus-power $2))
     ((MINUS power) (minus-power $2))
     ((power) (power-expression $1)))
    (power
     ((atom POWER factor) (pow-exp $1 $3))
     ((primary) (primary-expression $1)))
    (primary
     ((atom) (atom-exp $1))
     ((primary OB expression CB) (list-idx $1 $3))
     ((primary OP CP) (func-call-noargs $1))
     ((primary OP arguments CP) (func-call $1 $3)))
    (arguments
     ((expression) (an-argument $1))
     ((arguments COMMA expression) (some-arguments $1 $3)))
    (atom
     ((ID) (id-exp $1))
     ((NUM) (num-exp (num-val $1)))
     ((List) (list-expression $1))
     ((TRUE) (true-exp))
     ((FALSE) (false-exp))
     ((NONE) (none-exp)))
    (List
     ((OB CB) (null-list))
     ((OB expressions CB) (a-list $2)))
    (expressions
     ((expression) (an-exp $1))
     ((expressions COMMA expression) (some-exps $1 $3))))))

(provide (all-defined-out))