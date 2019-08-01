#lang racket


(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sum a b c)
  (if (> a b) (sum-of-squares a (if (> b c) b c))
              (sum-of-squares b (if (> a c) a c))))

(define (power-close-to b n)
  (define (is-greater e)
    (if (> (expt b e) n) e (is-greater (+ e 1))))
  (is-greater 0))


(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))