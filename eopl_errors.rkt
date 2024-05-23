#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")


(define var->string (lambda (var) (cases ATOM var
                                    (id-exp (name) name)
                                    (num-exp (num) (number->string (cases expval num (num-val (num) num) (else ""))))
                                    (list-expression (l) (string-append "list : " (list-exp->string l)))
                                    (true-exp () "True")
                                    (false-exp () "False")
                                    (none-exp () "None"))))

(define list-exp->string (lambda (l) (cases list-exp l
                                       (a-list (exps) (string-append "[" (expressions->string exps) "]"))
                                       (null-list () "[]"))))

(define expressions->string (lambda (exps) (cases expressions exps
                                             (an-exp (exp) (expression->string exp))
                                             (some-exps (exps1 exp) (string-append (expressions->string exps1) ", " (expression->string exp))))))

(define expression->string (lambda (exp) (cases expression exp
                                           (an-expression (dis) (disjunction->string dis)))))

(define disjunction->string (lambda (dis) (cases disjunction dis
                                            (a-disjunction (dis1 con) (string-append (disjunction->string dis1) " or " (conjunction->string con)))
                                            (conjunction-exp (con) (conjunction->string con)))))

(define conjunction->string (lambda (con) (cases conjunction con
                                            (a-conjunction (con1 inv) (string-append (conjunction->string con1) " and " (inversion->string inv)))
                                            (inversion-exp (inv) (inversion->string inv)))))

(define inversion->string (lambda (inv) (cases inversion inv
                                          (an-inversion (inv1) (string-append "not " (inversion->string inv1)))
                                          (comp-exp (comp) (comparison->string comp)))))

(define comparison->string (lambda (comp) (cases comparison comp
                                            (equal-sum (eq) (eq-exp->string eq))
                                            (lessthan-sum (lt) (lt-exp->string lt))
                                            (lessthanorequal-sum (let) (let-exp->string let))
                                            (greaterthan-sum (gt) (gt-exp->string gt))
                                            (greaterthanorequal-sum (get) (get-exp->string get))
                                            (sum-expression (sum) (sum->string sum)))))

(define eq-exp->string (lambda (eq) (cases eq-exp eq
                                      (an-eq-exp (num1 num2) (string-append (sum->string num1) " == " (sum->string num2))))))

(define lt-exp->string (lambda (lt) (cases lt-exp lt
                                      (a-lt-exp (num1 num2) (string-append (sum->string num1) " < " (sum->string num2))))))

(define let-exp->string (lambda (let) (cases let-exp let
                                      (a-let-exp (num1 num2) (string-append (sum->string num1) " <= " (sum->string num2))))))

(define gt-exp->string (lambda (gt) (cases gt-exp gt
                                      (a-gt-exp (num1 num2) (string-append (sum->string num1) " > " (sum->string num2))))))

(define get-exp->string (lambda (get) (cases get-exp get
                                      (a-get-exp (num1 num2) (string-append (sum->string num1) " >= " (sum->string num2))))))

(define sum->string (lambda (sum) (cases sum-exp sum
                                    (plus-term (num1 num2) (string-append (sum->string num1) " + " (term->string num2)))
                                    (minus-term (num1 num2) (string-append (sum->string num1) " - " (term->string num2)))
                                    (term-expression (term) (term->string term)))))

(define term->string (lambda (term) (cases term-exp term
                                      (times-factor (num1 num2) (string-append (term->string num1) " * " (factor->string num2)))
                                      (divides-factor (num1 num2) (string-append (term->string num1) " / " (factor->string num2)))
                                      (factor-expression (factor) (factor->string factor)))))

(define factor->string (lambda (factor) (cases factor-exp factor
                                          (plus-power (pow) (string-append "+" (power->string pow)))
                                          (minus-power (pow) (string-append "-" (power->string pow)))
                                          (power-expression (pow) (power->string pow)))))

(define power->string (lambda (pow) (cases power-exp pow
                                      (pow-exp (num1 num2) (string-append (var->string num1) " ** " (factor->string num2)))
                                      (primary-expression (prim) (primary->string prim)))))

(define primary->string (lambda (prim) (cases primary-exp prim
                                         (atom-exp (var) (var->string var))
                                         (list-idx (prim exp) (string-append (primary->string prim) "[" (expression->string exp) "]"))
                                         (func-call (prim args) (string-append (primary->string prim) "(" (args->string args) ")"))
                                         (func-call-noargs (prim) (string-append (primary->string prim) "()")))))

(define args->string (lambda (args) (cases arguments-exp args
                                      (an-argument (arg) (expression->string arg))
                                      (some-arguments (args arg) (string-append (args->string args) ", " (expression->string arg))))))

(define val->string (lambda (val) (cases expval val
                                    (num-val (num) 'number)
                                    (bool-val (bool) 'boolean)
                                    (list-val (array) 'list)
                                    (none-val () 'none))))

(define (report-no-binding-found! var) (eopl:error 'binding-dismatch "\n\tidentifier ~s is used before its declaration!" (var->string var)))

(define (report-invalid-reference!) (eopl:error 'invalid-reference "\n\tillegal reference to memory!"))

(define (report-no-program!) (eopl:error 'bad-syntax "\n\tthe given string is not a valid program to execute!"))

(define (report-invalid-lhs! var) (eopl:error 'invalid-LHS "\n\texpected a variable as left hand side of an assignment.\n\tprovided: ~s" (var->string var)))

(define (report-invalid-type! des pro) (eopl:error 'invalid-type "\n\texpected a ~s variable. provided: ~s" des (val->string pro)))

(define (report-divide-by-zero! exp) (eopl:error 'divide-by-zero "\n\tthe result of the divisor (~s) is zero." (factor->string exp)))

(define (report-index-out-of-bound! prim exp size) (eopl:error 'index-out-of-bound "\n\tthe value of ~s (~s) is out of bound for list with length ~s" (expression->string exp) size prim))

(define (report-invalid-cast! from to) (eopl:error 'invalid-cast "\n\tcannot cast from ~s to ~s datatype!" from to))

(define (report-too-many-arguments!) (eopl:error 'too-many-arguments "\n\ttoo many arguments in function call!"))

(provide (all-defined-out))