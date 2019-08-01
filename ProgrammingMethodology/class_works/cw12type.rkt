#lang typed/racket


(: prefixes (All (A) (-> (Listof A) (Listof (Listof A)))))
(define (prefixes xs)
  (if (null? xs)
      (list null)
      (cons null (map (lambda ([x : (Listof A)]) (cons (car xs) x))
                      (prefixes (cdr xs))))))

(define-type (Rose-Trees t) (U Leaf (Node t (Rose-Trees t))))
(define-type Leaf 'leaf)
(define-type (Node A B) (List 'node A (Listof B)))