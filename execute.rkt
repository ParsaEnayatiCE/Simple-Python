#! usr/bin/env racket
#lang racket

(require "interpreter.rkt")

(define my-program (make-parameter #false))

(define parser
  (command-line
   #:usage-help
   ""

   #:once-each
   [("-a" "--argument") ARG
                    "Set the program"
                    (my-program ARG)]

   #:args () (void)))

(define (run-program pgm)
  (cond
    [(boolean? pgm) "There is nothing to execute!"]
    [(string? pgm) (begin (run pgm) "")]))

(printf "~a\n" (run-program (my-program)))