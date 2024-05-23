#lang racket

(require "eopl_errors.rkt")
(require "env_store.rkt")
(require "datatypes.rkt")
(require "parser.rkt")
(require "lexer.rkt")
(require (lib "eopl.ss" "eopl"))
(require try-catch)


(define value-of-exp-thunk (lambda (th) (cases exp-thunk th (expression-thunk (exp glob_env curr_env) (value-of-expression exp glob_env curr_env)))))
(define value-of-func-thunk (lambda (th args glob_env curr_env) (cases func-thunk th (function-thunk (func) (value-of-function func args glob_env curr_env)))))

(define expval->bool (lambda (val) (cases expval val (bool-val (bool) bool) (else (report-invalid-type! 'bool val)))))
(define expval->num (lambda (val) (cases expval val (num-val (num) num) (else (report-invalid-type! 'number val)))))
(define expval->list (lambda (val) (cases expval val (list-val (array) array) (else (report-invalid-type! 'list val)))))

(define listexp->racketlist (lambda (array) (cond [(null? array) null]
                                                  [else (append (list (expval->racketval (car array))) (listexp->racketlist (cdr array)))])))

(define expval->racketval (lambda (val) (cases expval val (bool-val (bool) (if bool "True" "False")) (num-val (num) num) (list-val (array)
                                                          (listexp->racketlist array)) (none-val () ""))))

(define is-print (lambda (exp) (cases expression exp (an-expression (dis) (cases disjunction dis (conjunction-exp (con) (cases conjunction con
                                    (inversion-exp (inv) (cases inversion inv (comp-exp (comp) (cases comparison comp (sum-expression (sum)
                                    (cases sum-exp sum (term-expression (term) (cases term-exp term (factor-expression (factor) (cases factor-exp factor
                                    (power-expression (pow) (cases power-exp pow (primary-expression (prim) (cases primary-exp prim
                                    (func-call (prima args) (cases primary-exp prima (atom-exp (var) (cases ATOM var (id-exp (name) (if (equal? name "print") prim #f))
               (else #f))) (else #f))) (else #f))) (else #f))) (else #f))) (else #f))) (else #f))) (else #f))) (else #f))) (else #f))) (else #f))))))

(define primary->ATOM (lambda (val) (cases primary-exp val (atom-exp (var) var) (else (report-invalid-cast! 'Primary 'ATOM)))))
 
(define is-list (lambda (val) (cases expval val (list-val (array) #t) (else #f))))
(define is-bool (lambda (val) (cases expval val (bool-val (bool) #t) (else #f))))
(define is-num (lambda (val) (cases expval val (num-val (num) #t) (else #f))))


(define lex (lambda (lexer input) (lambda () (lexer input))))
(define run (lambda (str) (value-of-program (full-parser (lex full-lexer (open-input-string str))))))
(define execute (lambda (file_input) (value-of-program (full-parser (lex full-lexer (open-input-file file_input))))))

; program
(define value-of-program (lambda (pgm) (cases program pgm
                                        (a-program (sttmnts) (value-of-statements sttmnts (empty-env) (empty-env)))
                                       (else (report-no-program!)))))
; statements
(define value-of-statements (lambda (sttmnts glob_env curr_env) (cases statements sttmnts
                                                    (a-statement (sttmnt) (value-of-statement sttmnt glob_env curr_env))
                                                    (some-statements (sttmnts sttmnt) (let ((res (value-of-statements sttmnts glob_env curr_env))) (cond
                                                                                        [(list? (car res)) (if (equal? 'return (caar res)) res
                                                                                                               (value-of-statement sttmnt glob_env (caddr res)))]
                                                                                        [else (value-of-statement sttmnt glob_env (caddr res))]))))))
; statement
(define value-of-statement (lambda (sttmnt glob_env curr_env) (cases statement sttmnt
                                                  (compound-statement (cmp) (value-of-compound cmp glob_env curr_env))
                                                  (simple-statement (smpl) (value-of-simple smpl glob_env curr_env)))))
; simple
(define value-of-simple (lambda (smpl glob_env curr_env) (cases simple smpl
                                             (assignment-statement (assign) (value-of-assignment assign glob_env curr_env))
                                             (global-statement (glbl) (value-of-global glbl glob_env curr_env))
                                             (return-statement (rtrn) (value-of-return rtrn glob_env curr_env))
                                             (pass-statement () (list 'pass glob_env curr_env))
                                             (break-statement () (list 'break glob_env curr_env))
                                             (continue-statement () (list 'continue glob_env curr_env)))))

(define value-of-compound (lambda (cmp glob_env curr_env) (cases compound cmp
                                              (function-definition (func) (value-of-function-def func glob_env curr_env))
                                              (if-statement (if-stmt) (value-of-if if-stmt glob_env curr_env))
                                              (for-statement (for-stmt) (value-of-for for-stmt glob_env curr_env)))))
; needs try-catch package to run the code !
; assignment
(define value-of-assignment (lambda (assign glob_env curr_env) (cases assignment assign
                                                   (an-assignment (var exp) (cases ATOM var
                                                                              (id-exp (name) (try [(let ((loc (apply-env var curr_env))
                                                                                                         (func (is-print exp)))
                                                                                                   (if func (begin
                                                                                                   (setref! loc (value-of-primary func glob_env curr_env))
                                                                                                   (list 'assignment glob_env curr_env))
                                                                                                   (begin (setref! loc (expression-thunk exp glob_env curr_env))
                                                                                                   (list 'assignment glob_env curr_env))))]
                                                                                                  [catch (void (let ((func (is-print exp)))
                                                                                             (if func
                                                                                             (let ((loc (newref (value-of-primary func glob_env curr_env))))
                                                                                             (list 'assignment glob_env (extend-env var loc curr_env)))
                                                                                             (let ((loc (newref (expression-thunk exp glob_env curr_env))))
                                                                                             (list 'assignment glob_env (extend-env var loc curr_env))))))]))
                                                                              (else (report-invalid-lhs! var)))))))

(define value-of-global (lambda (glbl glob_env curr_env) (cases global glbl
                                                           (a-global (var) (list 'global glob_env (extend-env var (apply-env var glob_env) curr_env))))))

(define value-of-return (lambda (rtrn glob_env curr_env) (cases return rtrn
                                                           (void-return () (list (list 'return (none-exp)) glob_env curr_env))
                                                           (exp-return (exp) (list (list 'return (value-of-expression exp glob_env curr_env)) glob_env curr_env)))))

(define value-of-function-def (lambda (func glob_env curr_env) (cases function func
                                                   (function-statement (name pars body) (list 'func glob_env (extend-env name (newref
                                                   (function-thunk (function-statement name pars (some-statements body (simple-statement (return-statement (void-return)))))
                                                               )) curr_env)))
                                                   (function-statement-noargs (name body) (list 'func glob_env (extend-env name (newref
                                                   (function-thunk (function-statement-noargs name (some-statements body (simple-statement (return-statement (void-return)))))
                                                               )) curr_env))))))
; for
(define value-of-for (lambda (for glob_env curr_env) (cases for-exp for
                                                       (a-for-exp (var iterator body) (value-of-for-helper var (expval->list (value-of-expression iterator glob_env curr_env)) body glob_env curr_env)))))
; for
(define value-of-for-helper (lambda (var iterator body glob_env curr_env) (cond [(null? iterator) (list 'for glob_env curr_env)]
                                                                                  [else (let ((loc (newref (car iterator))))
                                                                                  (let ((res (value-of-for-body body iterator glob_env (extend-env var loc curr_env))))
                                                                            (value-of-for-helper var (cdar res) body glob_env curr_env)))])))
; for
(define value-of-for-body (lambda (body iterator glob_env curr_env) (cases statements body
                                                    (a-statement (sttmnt) (list iterator (value-of-statement sttmnt glob_env curr_env)))
                                                    (some-statements (sttmnts sttmnt) (let ((res (value-of-for-body sttmnts iterator glob_env curr_env))) (cond
                                                                                        [(equal? 'continue (car res)) (list iterator res)]
                                                                                        [(equal? 'break (car res)) (list '(null) res)]
                                                                                        [else (value-of-statement sttmnt glob_env (caddr res))]))))))
; if
(define value-of-if (lambda (if-stmt glob_env curr_env) (cases if-exp if-stmt
                                                          (an-if-exp (exp stmnts else-stmnts) (let ((condition (expval->bool (value-of-expression exp glob_env curr_env))))
                                                                                                (if condition (value-of-statements stmnts glob_env curr_env)
                                                                                                    (cases else-exp else-stmnts (an-else-exp (stmnts2)
                                                                                                           (value-of-statements stmnts2 glob_env curr_env)))))))))
; expression
(define value-of-expression (lambda (exp glob_env curr_env) (cases expression exp
                                                              (an-expression (dis) (value-of-disjunction dis glob_env curr_env)))))
; disjunction
(define value-of-disjunction (lambda (dis glob_env curr_env) (cases disjunction dis
                                                                (a-disjunction (dis con) (let ((res (value-of-disjunction dis glob_env curr_env)))
                                                                                           (if (expval->bool res) (bool-val #t)
                                                                                         (value-of-conjunction con glob_env curr_env))))
                                                                (conjunction-exp (con) (value-of-conjunction con glob_env curr_env)))))
; conjunction
(define value-of-conjunction (lambda (con glob_env curr_env) (cases conjunction con
                                                               (a-conjunction (con inv) (let ((res (value-of-conjunction con glob_env curr_env)))
                                                                                          (if (expval->bool res) (value-of-inversion inv glob_env curr_env)
                                                                                              (bool-val #f))))
                                                               (inversion-exp (inv) (value-of-inversion inv glob_env curr_env)))))
; inversion
(define value-of-inversion (lambda (inv glob_env curr_env) (cases inversion inv
                                                             (an-inversion (inv) (not (value-of-inversion inv glob_env curr_env)))
                                                             (comp-exp (comp) (value-of-comparison comp glob_env curr_env)))))

(define value-of-comparison (lambda (comp glob_env curr_env) (cases comparison comp
                                                               (equal-sum (eq) (value-of-eq eq glob_env curr_env))
                                                               (lessthan-sum (lt) (value-of-lt lt glob_env curr_env))
                                                               (lessthanorequal-sum (let-e) (value-of-let let-e glob_env curr_env))
                                                               (greaterthan-sum (gt) (value-of-gt gt glob_env curr_env))
                                                               (greaterthanorequal-sum (get) (value-of-get get glob_env curr_env))
                                                               (sum-expression (sum) (value-of-sum sum glob_env curr_env)))))

(define value-of-eq (lambda (eq glob_env curr_env) (cases eq-exp eq
                                                     (an-eq-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                  (res2 (value-of-sum num2 glob_env curr_env)))
                                                                              (bool-val (= (expval->num res1) (expval->num res2))))))))

(define value-of-lt (lambda (lt glob_env curr_env) (cases lt-exp lt
                                                     (a-lt-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                 (res2 (value-of-sum num2 glob_env curr_env)))
                                                                             (bool-val (< (expval->num res1) (expval->num res2))))))))

(define value-of-gt (lambda (gt glob_env curr_env) (cases gt-exp gt
                                                     (a-gt-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                 (res2 (value-of-sum num2 glob_env curr_env)))
                                                                             (bool-val (> (expval->num res1) (expval->num res2))))))))

(define value-of-let (lambda (let-e glob_env curr_env) (cases let-exp let-e
                                                     (a-let-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                 (res2 (value-of-sum num2 glob_env curr_env)))
                                                                             (bool-val (<= (expval->num res1) (expval->num res2))))))))

(define value-of-get (lambda (get glob_env curr_env) (cases get-exp get
                                                     (a-get-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                 (res2 (value-of-sum num2 glob_env curr_env)))
                                                                             (bool-val (>= (expval->num res1) (expval->num res2))))))))

(define value-of-sum (lambda  (sum glob_env curr_env) (cases sum-exp sum
                                                        (plus-term (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                     (res2 (value-of-term num2 glob_env curr_env)))
                                                                                 (try [(list-val (append (expval->list res1) (expval->list res2)))]
                                                                                      [catch (void (num-val (+ (expval->num res1) (expval->num res2))))])))
                                                        (minus-term (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                      (res2 (value-of-term num2 glob_env curr_env)))
                                                                                  (num-val (- (expval->num res1) (expval->num res2)))))
                                                        (term-expression (term) (value-of-term term glob_env curr_env)))))

(define value-of-term (lambda (term glob_env curr_env) (cases term-exp term
                                                         (times-factor (num1 num2) (let ((res1 (value-of-term num1 glob_env curr_env)))
                                                                                     (if (= 0 (expval->num res1)) (num-val 0)
                                                                                         (let ((res2 (value-of-factor num2 glob_env curr_env)))
                                                                                           (num-val (* (expval->num res1) (expval->num res2)))))))
                                                         (divides-factor (num1 num2) (let ((res1 (value-of-term num1 glob_env curr_env)))
                                                                                       (if (= 0 (expval->num res1)) (num-val 0)
                                                                                           (let ((res2 (value-of-factor num2 glob_env curr_env)))
                                                                                             (num-val (* (expval->num res1) (expval->num res2)))))))
                                                         (factor-expression (factor) (value-of-factor factor glob_env curr_env)))))

(define value-of-factor (lambda (factor glob_env curr_env) (cases factor-exp factor
                                                             (plus-power (pow) (value-of-power pow glob_env curr_env))
                                                             (minus-power (pow) (let ((res (value-of-power pow glob_env curr_env)))
                                                                                 (num-val (- 0 (expval->num res)))))
                                                             (power-expression (pow) (value-of-power pow glob_env curr_env)))))

(define value-of-power (lambda (pow glob_env curr_env) (cases power-exp pow
                                                         (pow-exp (num1 num2) (let ((res1 (value-of-ATOM num1 glob_env curr_env)))
                                                                                (if (or (= 0 (expval->num res1)) (= 1 (expval->num res1)))
                                                                                    res1
                                                                                    (let ((res2 (value-of-factor num2 glob_env curr_env)))
                                                                                       (num-val (expt (expval->num res1) (expval->num res2)))))))
                                                         (primary-expression (prim) (value-of-primary prim glob_env curr_env)))))
; TODO
(define value-of-primary (lambda (primary glob_env curr_env) (cases primary-exp primary
                                                               (atom-exp (var) (value-of-ATOM var glob_env curr_env))
                                                               (list-idx (prim exp) (let ((res1 (expval->list (value-of-primary prim glob_env curr_env)))
                                                                                          (res2 (expval->num (value-of-expression exp glob_env curr_env))))
                                                                                      (if (< res2 (length res1)) (list-ref res1 res2)
                                                                                          (report-index-out-of-bound! (length res1) exp res2))))
                                                               (func-call (prim args) (let ((res (value-of-func-thunk (deref (apply-env (primary->ATOM prim) curr_env)) args glob_env curr_env)))
                                                               (cases primary-exp prim (atom-exp (var) (cases ATOM var
                                                               (id-exp (name) (if (equal? name "print") (display (string-append (~a (expval->racketval res)) "\n")) res)) (else res))) (else res))))
                                                               (func-call-noargs (prim) (let ((res (value-of-func-thunk (deref (apply-env (primary->ATOM prim) curr_env)) null glob_env curr_env)))
                                                               (cases primary-exp prim (atom-exp (var) (cases ATOM var
                                                               (id-exp (name) (if (equal? name "print") (display (string-append (~a (expval->racketval res)) "\n")) res)) (else res))) (else res)))))))

(define value-of-function (lambda (func args glob_env curr_env) (cases function func
                                 (function-statement (name pars body) (let ((updated_glob (update-glob-env glob_env curr_env))
                                                                            (pairs (if (null? args) (defactor-params pars) (update-params pars args))))
                                                                        (cadar (value-of-statements body updated_glob (update-curr-env (car pairs) (cadr pairs) updated_glob (function-env name func))))))
                                 (function-statement-noargs (name body) (cadar (value-of-statements body (update-glob-env glob_env curr_env) (function-env name func)))))))

(define update-params (lambda (pars args) (let ((res (defactor-params pars)) (arguments (extract-arguments args)))
                                            (list (car res) (merge-params arguments (cadr res))))))

(define merge-params (lambda (args exps) (cond [(null? args) exps]
                                       [(null? exps) (report-too-many-arguments!)]
                                       [else (append (list (car args)) (merge-params (cdr args) (cdr exps)))])))

(define update-curr-env (lambda (vars exps glob_env curr_env) (cond [(null? vars) curr_env]
                                                           [(null? exps) (report-no-binding-found! (car vars))]
                                                           [else (update-curr-env (cdr vars) (cdr exps) glob_env
                                                           (extend-env (car vars) (newref (expression-thunk (car exps) glob_env glob_env)) curr_env))])))

(define update-glob-env (lambda (glob_env curr_env) (cases environment curr_env
                                                      (empty-environment () glob_env)
                                                      (extend-environment (var val env) (update-glob-env (extend-env var val glob_env) env)))))

(define defactor-params (lambda (params) (cases parameters params
                                           (assignment-parameter (assign) (cases assignment assign (an-assignment (var exp) (list (list var) (list exp)))))
                                           (all-parameters (pars assign) (cases assignment assign (an-assignment (var exp) (let ((res (defactor-params pars)))
                                                                            (list (append (car res) (list var)) (append (cadr res) (list exp))))))))))

(define extract-arguments (lambda (args) (cases arguments-exp args
                                      (an-argument (exp) (list exp))
                                      (some-arguments (arguments exp) (append (extract-arguments arguments) (list exp))))))

(define value-of-ATOM (lambda (atom glob_env curr_env) (cases ATOM atom
                                                         (id-exp (name) (let ((loc (apply-env atom curr_env))) (let ((val (deref loc)))
                                                                          (if (expval? val) val (let ((res (value-of-exp-thunk val)))
                                                                                                  (begin (setref! loc res) res))))))
                                                         (num-exp (num) num)
                                                         (list-expression (l) (value-of-list l glob_env curr_env))
                                                         (true-exp () (bool-val #t))
                                                         (false-exp () (bool-val #f))
                                                         (none-exp () (none-val)))))

(define value-of-list (lambda (l glob_env curr_env) (cases list-exp l
                                                      (a-list (exps) (let ((res (value-of-expressions exps glob_env curr_env)))
                                                                       (list-val res)))
                                                      (null-list () (list-val null)))))

(define value-of-expressions (lambda (exps glob_env curr_env) (cases expressions exps
                                                                (an-exp (exp) (let ((res (value-of-expression exp glob_env curr_env)))
                                                                                (list res)))
                                                                (some-exps (expressions exp) (let ((res1 (value-of-expressions expressions glob_env curr_env))
                                                                                                   (res2 (value-of-expression exp glob_env curr_env)))
                                                                                               (append res1 (list res2)))))))

(provide (all-defined-out))