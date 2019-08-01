#lang racket
(require rackunit)


(define identity
  (lambda (x) x))

(define square
  (lambda (x) (* x x)))

(define cube
  (lambda (x) (* x x x)))

(define (fast-power x n)
  (cond ((= n 0) 1.0)
        ((even? n) (square (fast-power x (/ n 2.0))))
        (else (* x (square (fast-power x (/ (- n 1) 2)))))))
      
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated p n)
  (if (= n 0)
      identity
      (compose p (repeated p (- n 1)))))
      



(define average
  (lambda (x y) (/ (+ x y) 2.0)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define log2
  (lambda (x) (/ (log x) (log 2))))

(define (fixed-point f first-guess)
  (define eps 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) eps))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))



(define (nth-root x n)
  (define f
    (lambda (y) (/ x (expt y (- n 1)))))
  (if (= x 0.0)
      0.0
      (if (and (< x 0.0) (even? n))
          (error "Pierwiastek stopnia parzystego z liczby ujemnej!")
          (fixed-point ((repeated average-damp (floor (log2 n))) f) 1.01))))

;Zastosowana liczba tlumien w kazdym przypadku to najmniejsza liczba calkowita c,
;taka, ze c = log2 n, gdzie n to stopien pierwiastka
;Dla n, ktory jest potega dwojki liczba tlumien moze byc wymagana mniejsza liczba tlumien



(define (run-tests)
  (define eps 0.00001)
  (check-= (nth-root 16 4) 2.0 eps)
  (check-= (nth-root 0.0 5) 0.0 eps)
  ;(check-= (nth-root -1.0 4) -1.0 eps)
  (check-= (nth-root -1.0 3) -1.0 eps)
  (check-= (nth-root 8.0 3) 2.0 eps)
  (check-= (nth-root 2.0 4) 1.1892 eps)
  ;(check-= (nth-root -2.0 4) -1.1892 eps)
  (check-= (nth-root 0.0000000000421 5) 0.00841 eps)
  (check-= (nth-root 2147483648.0 31) 2.0 eps)
  (check-= (nth-root -2147483648.0 31) -2.0 eps)
  (check-= (nth-root 1.8437 5) 1.13015 eps)
  (check-= (nth-root 82.12812 6) 2.08489 eps)
  (check-= (nth-root -229219129 15) -3.60869 eps)
  (check-= (nth-root 0.0002569 10) 0.43749 eps)
  (check-= (nth-root 9218429291491294 20) 6.28395 eps)
  (check-= (nth-root 9482217.2182178 10) 4.98529 eps))

(run-tests)

