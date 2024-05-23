#lang racket

(require (lib "eopl.ss" "eopl"))

(define-datatype program program?
  (a-program (stmnts statements?)))

(define-datatype statements statements?
  (a-statement (stmnt statement?))
  (some-statements (stmnts statements?) (stmnt statement?)))

(define-datatype statement statement?
  (compound-statement (comp compound?))
  (simple-statement (smpl simple?)))

(define-datatype simple simple?
  (assignment-statement (assign assignment?))
  (global-statement (glbl-stmnt global?))
  (return-statement (rtrn-stmnt return?))
  (pass-statement)
  (break-statement)
  (continue-statement))

(define-datatype compound compound?
  (function-definition (func function?))
  (if-statement (if-stmnt if-exp?))
  (for-statement (for-stmnt for-exp?)))

(define-datatype assignment assignment?
  (an-assignment (var ATOM?) (exp expression?)))

(define-datatype return return?
  (void-return)
  (exp-return (exp expression?)))

(define-datatype global global?
  (a-global (var ATOM?)))

(define-datatype function function?
  (function-statement (name ATOM?) (pars parameters?) (body statements?))
  (function-statement-noargs (name ATOM?) (body statements?)))

(define-datatype parameters parameters?
  (assignment-parameter (assign assignment?))
  (all-parameters (params parameters?) (assign assignment?)))

(define-datatype if-exp if-exp?
  (an-if-exp (exp expression?) (stmnts statements?) (else-stmnt else-exp?)))

(define-datatype else-exp else-exp?
  (an-else-exp (stmnts statements?)))

(define-datatype for-exp for-exp?
  (a-for-exp (var ATOM?) (iterator expression?) (body statements?)))

(define-datatype expression expression?
  (an-expression (disjunc disjunction?)))

(define-datatype disjunction disjunction?
  (a-disjunction (disj disjunction?) (conj conjunction?))
  (conjunction-exp (conj conjunction?)))

(define-datatype conjunction conjunction?
  (a-conjunction (conj conjunction?) (inv inversion?))
  (inversion-exp (inv inversion?)))

(define-datatype inversion inversion?
  (an-inversion (inv inversion?))
  (comp-exp (comp comparison?)))

(define-datatype comparison comparison?
  (equal-sum (eq eq-exp?))
  (lessthan-sum (lt lt-exp?))
  (lessthanorequal-sum (let let-exp?))
  (greaterthan-sum (gt gt-exp?))
  (greaterthanorequal-sum (get get-exp?))
  (sum-expression (sum sum-exp?)))

(define-datatype eq-exp eq-exp?
  (an-eq-exp (num1 sum-exp?) (num2 sum-exp?)))

(define-datatype lt-exp lt-exp?
  (a-lt-exp (num1 sum-exp?) (num2 sum-exp?)))

(define-datatype let-exp let-exp?
  (a-let-exp (num1 sum-exp?) (num2 sum-exp?)))

(define-datatype gt-exp gt-exp?
  (a-gt-exp (num1 sum-exp?) (num2 sum-exp?)))

(define-datatype get-exp get-exp?
  (a-get-exp (num1 sum-exp?) (num2 sum-exp?)))

(define-datatype sum-exp sum-exp?
  (plus-term (num1 sum-exp?) (num2 term-exp?))
  (minus-term (num1 sum-exp?) (num2 term-exp?))
  (term-expression (term term-exp?)))

(define-datatype term-exp term-exp?
  (times-factor (num1 term-exp?) (num2 factor-exp?))
  (divides-factor (num1 term-exp?) (num2 factor-exp?))
  (factor-expression (factor factor-exp?)))

(define-datatype factor-exp factor-exp?
  (plus-power (pow power-exp?))
  (minus-power (pow power-exp?))
  (power-expression (pow power-exp?)))

(define-datatype power-exp power-exp?
  (pow-exp (num1 ATOM?) (num2 factor-exp?))
  (primary-expression (prim primary-exp?)))

(define-datatype primary-exp primary-exp?
  (atom-exp (var ATOM?))
  (list-idx (prim primary-exp?) (exp expression?))
  (func-call (prim primary-exp?) (args arguments-exp?))
  (func-call-noargs (prim primary-exp?)))

(define-datatype arguments-exp arguments-exp?
  (an-argument (exp expression?))
  (some-arguments (args arguments-exp?) (exp expression?)))

(define-datatype ATOM ATOM?
  (id-exp (name string?))
  (num-exp (num expval?))
  (list-expression (l list-exp?))
  (true-exp)
  (false-exp)
  (none-exp))

(define-datatype list-exp list-exp?
  (a-list (exps expressions?))
  (null-list))

(define-datatype expressions expressions?
  (an-exp (exp expression?))
  (some-exps (exps expressions?) (exp expression?)))

; Thunk (Lazy evaluation)
(define-datatype exp-thunk exp-thunk?
 (expression-thunk (exp expression?) (glob_env environment?) (curr_env environment?)))
(define-datatype func-thunk func-thunk?
  (function-thunk (func function?)))

(define-datatype expval expval?
 (num-val (num number?))
 (bool-val (bool boolean?))
 (list-val (array list?))
 (none-val))

(define-datatype func func?
  (a-func (name ATOM?) (args arguments-exp?) (body statements?)))

(define-datatype for for?
  (a-for (var ATOM?) (iterator list-exp?) (body statements?)))

(define-datatype environment environment?
  (empty-environment)
  (extend-environment (var ATOM?) (val expval?) (env environment?)))

(provide (all-defined-out))