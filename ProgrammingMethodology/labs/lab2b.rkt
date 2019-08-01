#lang racket
(require rackunit)


(define identity
  (lambda (x) x))
(define square
  (lambda (x) (* x x)))

(define cube
  (lambda (x) (* x x x)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated p n)
  (if (= n 0)
      (identity p)
      (compose p (repeated p (- n 1)))))
      
(define (atan-num n x)
  (cond ((= n 1) x)
        ((= n 2) (square x))
        (else (square (* (- n 1) x)))))

(define (atan-den n x)
  (- (* 2 n) 1))


(define pi-num1
  (lambda (x y) (square(- (* x 2.0) 1.0))))

(define (pi-den1 n x)
  6.0)

(define (pi-num2 n x)
  (if (= n 1)
      4
      (square (- n 1))))

(define (pi-den2 n x)
  (- (* 2 n) 1))
  
     

(define (icf num den x)
  (define eps 0.00001)
  (define (ic-iter a b c d n acc)
    (define (f a b)
        (+ (* (num n x) a)
           (* (den n x) b)))
    (let ([nextA (f a b)]
          [nextB (f c d)])
      (define nextAcc (/ nextA nextB))
      (if (< (abs(- nextAcc acc)) eps)
          nextAcc
          (ic-iter b nextA d nextB (+ n 1) nextAcc))))
  (ic-iter 1 0 0 1 1 0.0))

;Funckje testowe, w ktorych wartosc x jest nieistotna, x jest ustawiona na 1.0
(define (run-tests)
  (define eps 0.00001)
  ; Ulamek lancuchowy pi z zadania 7
  (check-= (+ 3.0 (icf pi-num1 pi-den1 1.0)) 3.14159 eps)
  ;inny ulamek lancuchowy dla pi
  (check-= (icf  pi-num2 pi-den2 1.0) 3.14159 eps)
  ;porownywanie uzycia funkcji na ulamku lancuchowym atan x z zadania 8 i wbudowanej funkcji
  (check-= (icf  atan-num atan-den 2.0) (atan 2.0) eps)
  (check-= (icf  atan-num atan-den -1.0) (atan -1.0) eps)
  (check-= (icf  atan-num atan-den 4.421) (atan 4.421) eps)
  (check-= (icf  atan-num atan-den 0.0094) (atan 0.0094) eps)
  ;odwrotnosc zlotej liczby
  (check-= (icf ( lambda (x y) 1.0) ( lambda (x y) 1.0) 1.0) 0.61803 eps)
  ;zlota liczba
  (check-= (/ 1 (icf ( lambda (x y) 1.0) ( lambda (x y) 1.0) 1.0)) 1.61803 eps))
  
(run-tests)

