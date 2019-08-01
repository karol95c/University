#lang racket

(define identity
  (lambda (x) x))
(define square
  (lambda (x) (* x x)))

(define cube
  (lambda (x) (* x x x)))

(define (fast-power x n)
  (cond ((= n 0) 1.0)
        ((even? n) (square (fast-power x (/ n 2.0))))
        (else (* x (square (fast-power x (/ (- n 1) 2.0)))))))
      

  

(define inc
  (lambda (x) (+ x 1)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated p n)
  (if (= n 0)
      (identity p)
      (compose p (repeated p (- n 1)))))
      
        
(define mult
  (lambda (x y) (* x y)))

(define (product-iter f a b next)
  (define (iter i acc)
    (if (> i b)
        acc
        (iter (next i) (* acc (f i)))))
  (iter a 1.0))

(define (product-rec term a b next)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) b  next))))

;ogolna funkcja iteracyjna (combiner-dzialanie null-value-wartosc startowa(neutralna)
(define (accumulate-iter combiner null-value term a next b)
  (define (iter x acc)
    (if (> x b)
        acc
        (iter (next x) (combiner (term x) acc))))
    (iter a null-value))

;ogolna funkcja rekurencyjna (combiner-dzialanie null-value-wartosc startowa(neutralna)
(define (accumulate-rec combiner null-value term a b next)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) b next))))


(define (pi-div4 a b)
  (define (pi-term x)
    (/ (* x (+ 2 x)) (square (+ x 1))))
  (define (pi-next x)
    (+ 2 x))
  ;(product-iter pi-term a b pi-next ))
   (accumulate-rec * 1.0 pi-term a b pi-next))


(define (cont-frac-rec num den k)
  (define (helper x)
    (if (= x k)
        1.0
        (/ (num x) (+ (den x)(helper (+ x 1))))))
  (helper 1))

(define (cont-frac-iter num den k acc)
  (if (= k 0)
      acc
      (cont-frac-iter num den (- k 1) (/ (num k) (+ (den k) acc)))))


( define ( build n d b)
   (/ n (+ d b)))

; (repeated-build x n d b)
;  (if (= x 0)
;      1.0

(define (icf num den)
  (define eps 0.000001)
  (define (ic-iter x a b c d n acc)
    (define (f a b)
        (+ (* (den n) a)
           (* (num n) b)))
    (define nextA (f a b))
    (define nextB (f c d))
    (define nextAcc (/ nextA nextB))
    (if (< (abs(- nextAcc acc)) eps)
          nextAcc
          (ic-iter x b nextA d nextB (+ n 1) nextAcc)))
  (ic-iter 1.0 1 0 0 1 1 0.0))

;(/ 1(icf ( lambda (i) 1.0) ( lambda (i) 1.0)))
;(+ 3.0 (icf ( lambda (x) 6.0) ( lambda (x) (square(- (* x 2.0) 1.0)))))


(define (atan-den n)
  (* 2. (- 1 n)))
;(define (atan-den n)
 ; (if (= n 0) x 


(define tolerance 0.00001)

(define average
  (lambda (x y) (/ (+ x y) 2.0)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (four-root x)
  (define f
    (lambda (y) (/ x (cube y))))
  (fixed-point (repeated (average-damp f) 2) 1.0))

(define (nth-root x n)
  (define f
    (lambda (y) (/ x (fast-power y (- n 1)))))
  (fixed-point (repeated (average-damp f) 2) 1.0))
  
;(nth-root 16 4)

  
    
        
      


;OBLICZENIA
;(* 4.0 (pi-div4 2.0 10000))
;(cont-frac-iter ( lambda (i) 1.0) ( lambda (i) 1.0) 2 0.0)
;(cont-frac-rec ( lambda (i) 1.0) ( lambda (i) 1.0) 2)
(product-iter square 0 5 inc)

