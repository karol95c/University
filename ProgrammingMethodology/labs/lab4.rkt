#lang racket
(require racket/math)

(define (inc n)
  (+ n 1))

;;; ordered elements
(define (make-elem pri val)
  (cons pri val))

(define (elem-priority x)
  (car x))

(define (elem-val x)
  (cdr x))

;;; leftist heaps (after Okasaki)

;; data representation
(define leaf 'leaf)

(define (leaf? h) (eq? 'leaf h))

(define (hnode? h)
  (and (list? h)
       (= 5 (length h))
       (eq? (car h) 'hnode)
       (natural? (caddr h))))

(define (make-node elem heap-a heap-b)
  (let [(ra (rank heap-a))
        (rb (rank heap-b))]
    (if (> ra rb)
        (list 'hnode elem (+ 1 rb) heap-a heap-b)
        (list 'hnode elem (+ 1 ra) heap-b heap-a))))

(define (node-elem h)
  (second h))

(define (node-left h)
  (fourth h))

(define (node-right h)
  (fifth h))

(define (hord? p h)
  (or (leaf? h)
      (<= p (elem-priority (node-elem h)))))

(define (heap? h)
  (or (leaf? h)
      (and (hnode? h)
           (heap? (node-left h))
           (heap? (node-right h))
           (<= (rank (node-right h))
               (rank (node-left h)))
           (= (rank h) (inc (rank (node-right h))))
           (hord? (elem-priority (node-elem h))
                  (node-left h))
           (hord? (elem-priority (node-elem h))
                  (node-right h)))))

(define (rank h)
  (if (leaf? h)
      0
      (third h)))

;; operations

(define empty-heap leaf)

(define (heap-empty? h)
  (leaf? h))

(define (heap-insert elt heap)
  (heap-merge heap (make-node elt leaf leaf)))

(define (heap-min heap)
  (node-elem heap))

(define (heap-pop heap)
  (heap-merge (node-left heap) (node-right heap)))

(define (heap-merge h1 h2)
  (cond
   [(leaf? h1) h2]
   [(leaf? h2) h1]
   [(< (elem-val (node-elem h1)) (elem-val (node-elem h2)))
    (make-node (node-elem h1) (node-left h1)
               (heap-merge (node-right h1) h2))]
   [else  (make-node (node-elem h2) (node-left h2)
                     (heap-merge (node-right h2) h1))]))


;;; heapsort. sorts a list of numbers.
(define (heapsort xs)
  (define (popAll h)
    (if (heap-empty? h)
        null
        (cons (elem-val (heap-min h)) (popAll (heap-pop h)))))
  (let ((h (foldl (lambda (x h)
                    (heap-insert (make-elem x x) h))
            empty-heap xs)))
    (popAll h)))

;;; check that a list is sorted (useful for longish lists)
(define (sorted? xs)
  (cond [(null? xs)              true]
        [(null? (cdr xs))        true]
        [(<= (car xs) (cadr xs)) (sorted? (cdr xs))]
        [else                    false]))

;;; generate a list of random numbers of a given length
(define (randlist len max)
  (define (aux len lst)
    (if (= len 0)
        lst
        (aux (- len 1) (cons (random max) lst))))
  (aux len null))

;; Funckja test zwraca przyblizony czas w milisekundach jaki byl potrzebny na sortowanie jesli lista
;; jest posortowana, false w przeciwnym przypadku
(define (test xs)
  (define t1 (current-inexact-milliseconds))
  (define l (heapsort xs))
  (define t2 (current-inexact-milliseconds))
  (if (sorted? l)
      (- t2 t1)
      false))
    
(test (randlist 100 1000))
(test (randlist 100 10000))
(test (randlist 1000 500000))
(test (randlist 1000 4294967087))
(test (randlist 5000 4294967087))
(test (randlist 10000 4294967087))
(test (randlist 100000 4294967087))

