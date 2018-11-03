#lang racket

(define (make-rat n d)
  (cons n (cons d null)))

(define (rat-num x)
  (car x))

(define (rat-den x)
  (car ( cdr x)))

(define (rat? x)
  (and (not (equal? (car (cdr x)) 0))
       (not (equal? (car x) null))
       (not (equal? (cdr x) null))
       (not (pair? (cdr (cdr x))))
       (equal? (cdr (cdr x)) null)))

;(define x (make-rat 8 2))
;(define y (make-rat 8 (make-rat 4 2)))

;(rat? x)

(define (make-vect a b)
  (if (and (point? a) (point? b))
      (cons a b)
      (error "Nie mozna utworzyc wektora!")))

(define (vect? v)
  (and (point? (car v)) (point? (cdr v))))


(define (vect-begin v)
  (if (vect? v) 
      (car v)
      (error "Element nie jest wektorem!")))

(define (vect-end v)
  (if (vect? v) 
      (cdr v)
      (error "Element nie jest wektorem!")))

(define (make-point x y)
  (if (not (and (pair? x) (pair? y) (equal? x null) (equal? y null)))
      (cons x y)
      (error "Nie mozna utworzyc punktu!")))
      

(define (point? p)
  (not (and (pair? (car p)) (pair? (cdr p)) (equal? (car p) null) (equal? (cdr p) null))))

(define (point-x p)
  (if (point? p)
      (car p)
      (error "Element nie jest punktem!")))
(define (point-y p)
  (if (point? p)
      (cdr p)
      (error "Element nie jest punktem!")))

( define ( display-point p)
   ( display "(")
   ( display ( point-x p))
   ( display ", ")
   ( display ( point-y p))
   ( display ")"))

( define ( display-vect v)
   ( display "[")
   ( display-point ( vect-begin v))
   ( display ", ")
   ( display-point ( vect-end v))
   ( display "]"))


(define (vect-length v)
  (let ([x (car (vect-cord v))]
        [y (cdr (vect-cord v))])
    (sqrt (+ (expt x 2) (expt y 2)))))

(define (vect-scale v k)
  (let ([x (* k (car (vect-cord v)))]
        [y (* k (cdr (vect-cord v)))])
    (make-vect (vect-begin v) (make-point (+ (point-x (vect-begin v)) x) (+ (point-y (vect-begin v)) y)))))

(define (vect-translate v p)
  (let ([x (car (vect-cord v))]
        [y (cdr (vect-cord v))])
    (make-vect p (make-point (+ (point-x p) x) (+ (point-y p) y)))))

(define (vect-cord v)
  (let ([xa (point-x (vect-begin v))]
        [ya (point-y (vect-begin v))]
        [xb (point-x (vect-end v))]
        [yb (point-y (vect-end v))])
    (cons (- xb xa) (- yb ya))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse-rec list1)
  (if (null? list1)
      null
      (append (reverse-rec (cdr list1)) (list (car list1)))))

(define (reverse-iter n)
  (define (iter a b)
    (if (null? a)
        b
        (iter (cdr a) (cons (car a) b))))
  (iter n null))


(define (insert xs n)
  (cond
    [(null? xs) (cons n xs)]
   ; [(not (pair? (cdr xs)))
    ; (if (<= n (cdr xs))
     ;    (cons n (cdr xs))
    [(<= n (car xs)) (cons n xs)]
    (else (cons (car xs) (insert (cdr xs) n)))))
    ;(else (cons (car xs) (insert (cdr xs) n)))))

(define (insert-sort xs)
  (if (null? xs)
      null
      (insert (car xs) (insert-sort (cdr xs)))))
         
         ;(insert (car xs) (insert-sort (cdr xs)))))

      

;(define p1 (make-point 0 0))
;(define p2 (make-point 1 1))
;(define v1 (make-vect p1 p2))
;(define p3 (make-point 0 3))
;(display-vect v1)
;(vect-length v1)
;(define v2 (vect-translate v1 p3))
;(display-vect v2)
;(vect-length v2)
(define lista (list 1 2 3 4 6 7 8))
(insert lista 9)
(reverse-rec (insert lista 5))
(define lista3 (list 3 4))
(define lista2 (list 3 8 4 2 10 4 11))
(insert-sort lista2)
  