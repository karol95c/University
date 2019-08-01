#lang racket
(require rackunit)


(define (cube-root x)
  (define (square x) (* x x))
  (define eps 0.00001)
  (define (cube x) (* x x x))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) eps))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (if (= x 0.0)
      0.0
      (cube-root-iter 1.0)))


       
 (define (run-tests)
   (define eps 0.00001)
   (check-= (cube-root  3.0) 1.44225 eps)
   (check-= (cube-root  0.0) 0.0 eps)
   (check-= (cube-root  -3.0) -1.44225 eps)
   (check-= (cube-root  1.0) 1.0 eps)
   (check-= (cube-root  15.0) 2.46621 eps)
   (check-= (cube-root  10.23) 2.17083 eps)
   (check-= (cube-root  2.439) 1.34608 eps)
   (check-= (cube-root  28.018) 3.03724 eps)
   (check-= (cube-root  2029) 12.65981 eps)
   (check-= (cube-root  -298.18) -6.68076 eps)
   (check-= (cube-root  -2128.108) -12.86267 eps)
   (check-= (cube-root  10909.02) 22.17832 eps)
   (check-= (cube-root  99999.99999) 46.41589 eps)
   ;W dwoch ostatnich przypadkach wystepuja bledy numeryczne
   ;Funkcja nie oblicza prawidlowego wyniku
   (check-= (cube-root 0.0000025) 0.0135721 eps)
   (check-= (cube-root  0.00000000025) 0.00063 eps))
