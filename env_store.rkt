#lang racket

(require "eopl_errors.rkt")
(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define the-store '())

(define get-store (lambda () the-store))

(define newref (lambda (val) (let ((ref (length the-store))) (and (set! the-store (append the-store (list val))) (num-val ref)))))

(define deref (lambda (ref) (let ((size (length the-store))) (cases expval ref (num-val (num) (if (< num size) (list-ref the-store num) (report-invalid-reference!)))
                                                               (else (none-exp))))))

(define setref! (lambda (ref val) (let ((size (length the-store))) (cases expval ref (num-val (num) (if (< num size)
                                                                      (set! the-store (update num val the-store '())) (report-invalid-reference!)))
                                                                     (else (none-exp))))))

(define update (lambda (num val store current) (cond [(= 0 num) (append current (list val) (cdr store))]
                                             [else (update (- num 1) val (cdr store) (append current (list (car store))))])))

;--------------------------------------------------------

(define PRINT_FUNC (function-thunk
(function-statement
     (id-exp "print")
     (assignment-parameter
      (an-assignment
       (id-exp "print_func")
       (an-expression
        (conjunction-exp
         (inversion-exp (comp-exp (sum-expression (term-expression (factor-expression (power-expression (primary-expression (atom-exp (none-exp)))))))))))))
     (a-statement
      (simple-statement
       (return-statement
        (exp-return
         (an-expression
          (conjunction-exp
           (inversion-exp
            (comp-exp (sum-expression (term-expression (factor-expression (power-expression (primary-expression (atom-exp (id-exp "print_func"))))))))))))))))))

(define extend-env (lambda (var val env) (extend-environment var val env)))

(define function-env (lambda (name func) (extend-env name (newref (function-thunk func)) (empty-env))))

(define empty-env (lambda () (extend-env (id-exp "print") (newref PRINT_FUNC) (empty-environment))))

(define apply-env (lambda (var env) (cases environment env
                                      (empty-environment () (report-no-binding-found! var))
                                      (extend-environment (saved-var val saved-env) (if (equal? var saved-var) val (apply-env var saved-env))))))

  
(provide (all-defined-out))