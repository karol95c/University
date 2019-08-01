#lang racket

(require racket/contract)

;(define sorts3/c
;  (->i ([l (listof integer?)])
;       [result (and/c (listof integer?) sorted?)]
;       #:post (l result)
;       (and
;        (contains? l result)
;        (contains? result l))))


(define/contract (dist x y)
  (-> number? number? number?)
  (abs (- x y)))

(define/contract (average x y)
  (-> number? number? number?)
  (/ (+ x y) 2))

(define/contract (square x)
  (-> number? number?)
  (* x x))


(define sqrt/c
  (->i ([l positive?])
       [result positive?]
       #:post (l result)
       (< (abs (- (* result result) l)) 0.0001)))

(define/contract (sqrt x)
  sqrt/c
  ;;(-> positive? positive?)
  ;; lokalne definicje
  ;; poprawienie przybliżenia pierwiastka z x
  (define (improve approx)
    (average (/ x approx) approx))
  ;; nazwy predykatów zwyczajowo kończymy znakiem zapytania
  (define (good-enough? approx)
    (< (dist x (square approx)) 0.0001))
  ;; główna procedura znajdująca rozwiązanie
  (define (iter approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))
  
  (iter 1.0))

(define filter/c
  (->i ([p any/c]
        [l (listof any/c)])
      [result (listof any/c)]
      #:post (p result)
      (andmap p result)))

(define/contract (filter p xs)
  (let ([a (new-∀/c 'a)])
    (and/c (-> (-> a boolean?) (listof a) (listof a)) filter/c))
  (cond [(null? xs) null]
        [(p (car xs)) (cons (car xs) (filter p (cdr xs)))]
        [else (filter p (cdr xs))]))


(define/contract (suffixes xs)
  (let ([a (new-∀/c 'a)])
    (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
      (cons xs (suffixes (cdr xs)))))




(define/contract (prefixes xs)
  (let ([a (new-∀/c 'a)])
    (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
      (cons null (map (lambda (x) (cons (car xs) x))
                      (prefixes (cdr xs))))))



;; sygnatura słowników bez kontraktów
;(define-signature dict^
;  (dict? dict-empty? empty-dict dict-insert dict-remove dict-lookup))

;; sygnatura słowników z prostymi kontraktami
;(define-signature dict^
;  ((contracted
;    [dict?       (-> any/c boolean?)]
;    [dict-empty? (-> dict? boolean?)]
;    [empty-dict  (and/c dict? dict-empty?)]
;    [dict-insert (-> dict? string? any/c dict?)]
;    [dict-remove (-> dict? string? dict?)]
;    [dict-lookup (-> dict? string?
;                     (or/c (cons/c string? any/c) #f))])))

;; sygnatura słowników z kontraktami zależnymi
(define-signature dict^
  ((contracted
    [dict?       (-> any/c boolean?)]
    [dict-empty? (-> dict? boolean?)]
    [empty-dict  (and/c dict? dict-empty?)]
    [dict-insert (->i ([d dict?]
                       [k string?]
                       [v any/c])
                      [result (and/c dict? (not/c dict-empty?))]
                      #:post (result k v)
                      (let ((p (dict-lookup result k)))
                        (and
                          (pair? p)
                          (eq? (car p) k)
                          (eq? (cdr p) v))))]
    [dict-remove (->i ([d dict?]
                       [k string?])
                      [result dict?]
                      #:post (result k)
                      (eq? #f (dict-lookup result k)))]
    [dict-lookup (->i ([d dict?]
                       [k string?])
                     (result (or/c (cons/c string? any/c) #f))
                     #:post (result d)
                     (if (dict-empty? d) (eq? #f result) #t))])))
    
;; implementacja słowników na listach
(define-unit dict-list@
  (import)
  (export dict^)

  (define (dict? d)
    (and (list? d)
         (eq? (length d) 2)
         (eq? (car d) 'dict-list)))

  (define (dict-list d) (cadr d))
  (define (dict-cons l) (list 'dict-list l))
  
  (define (dict-empty? d)
    (eq? (dict-list d) '()))

  (define empty-dict (dict-cons '()))

  (define (dict-lookup d k) (assoc k (dict-list d)))

  (define (dict-remove d k)
    (dict-cons (remf (lambda (p) (eq? (car p) k)) (dict-list d))))

  (define (dict-insert d k v)
    (dict-cons (cons (cons k v)
                     (dict-list (dict-remove d k))))))

;; otwarcie implementacji słownika
(define-values/invoke-unit/infer dict-list@)

;;implementacja monoidu

(define-signature monoid^
   ((contracted
      [elem? (-> any/c boolean?)]
      [neutral elem?]
      [oper
        (-> elem? elem? elem?)])))


(define-unit monoid-ints@
  (import)
  (export monoid^)

  (define (elem? m)
    (number? m))

  (define neutral 0)

  (define (oper m1 m2)
    (+ m1 m2)))


(define-unit monoid-lists@
  (import)
  (export monoid^)

  (define (elem? m)
    (list? m))

  (define neutral '())

  (define (oper m1 m2)
    (append m1 m2)))

          
;(define-values/invoke-unit/infer monoid-ints@)
(define-values/invoke-unit/infer monoid-lists@)