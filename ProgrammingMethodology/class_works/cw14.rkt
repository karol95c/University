#lang racket

;;; rozdział 3.1.1

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;: (withdraw 25)
;: (withdraw 25)
;: (withdraw 60)
;: (withdraw 15)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))


; (define W1 (make-withdraw 100))
; (define W2 (make-withdraw 100))
; (W1 50)
; (W2 70)
; (W2 40)
; (W1 40)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    'incorrect-password)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        incorrect-password))
  dispatch)

(define acc (make-account 100 'qwerty))

;((acc 'abc 'withdraw) 50)
;((acc 'qwerty 'withdraw) 60)
;((acc 'qwerty 'deposit) 40)
;((acc 'qwerty 'withdraw) 60)

;;; rozdział 3.1.3

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))


;: (define W (make-simplified-withdraw 25))
;: (W 20)
;: (W 10)


(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

; (define D (make-decrementer 25))
; (D 20)
 ;(D 10)

;: ((make-decrementer 25) 20)
;: ((lambda (amount) (- 25 amount)) 20)
;: (- 25 20)

;: ((make-simplified-withdraw 25) 20)

;: ((lambda (amount) (set! balance (- 25 amount)) 25) 20)
;: (set! balance (- 25 20)) 25

;;; Toższamość obiektów

;: (define D1 (make-decrementer 25))
;: (define D2 (make-decrementer 25))
;: 
;: (define W1 (make-simplified-withdraw 25))
;: (define W2 (make-simplified-withdraw 25))
;: 
;: (W1 20)
;: (W1 20)
;: (W2 20)

;: (define peter-acc (make-account 100))
;: (define paul-acc (make-account 100))
;: 
;: (define peter-acc (make-account 100))
;: (define paul-acc peter-acc)

;;; Kłopoty z programowaniem imperatywnym

(define (factorial1 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

(define (make-cycle xs)
  (define (iter ys)
    (if (null? (mcdr ys))
        (set-mcdr! ys xs)
        (iter (mcdr ys)))
    xs)
  (if (null? xs)
      (error "Pusta lista!")
      (iter xs)))



(define l (mcons 1 (mcons 2 (mcons 3 null))))
(define p (mcons 0 (make-cycle
(mcons 1 null))))
(define ls (mcons 1 (mcons 2 null)))

(define (has-cycle? xs)
  (define (race p1 p2)
    (if (or (null? p2)
            (null? (mcdr p2)))
        false
        (if (eq? (mcdr p1) (mcdr (mcdr p2)))
            true
            (race (mcdr p1)  (mcdr (mcdr p2))))))
  (race xs xs))


(define (make-monitored f)
  (let ((counter 0))
    (cons
     (lambda xs
       (begin
         (set! counter (+ counter 1))
         (apply f xs)))
    (lambda (x)
      (cond [(eq? x 'how-many?) counter]
            [(eq? x 'reset) (set! counter 0)]
            (else (error "incorrect tag!" )))))))

(define p1 ( make-monitored +))



    



