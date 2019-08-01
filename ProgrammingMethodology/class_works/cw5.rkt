#lang racket

(define (var? t)
  (symbol? t))

(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (conj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'conj (car t))))

(define (disj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))

(define (prop? f)
  (or (var? f)
      (and (neg? f)
           (prop? (neg-subf f)))
      (and (disj? f)
           (prop? (disj-left f))
           (prop? (disj-right f)))
      (and (conj? f)
           (prop? (conj-left f))
           (prop? (conj-right f)))))

(define (make-neg f)
  (list 'neg f))
(define (make-disj l r)
  (list 'disj l r))
(define (make-conj l r)
  (list 'conj l r))
(define (neg-subf f)
  (second f))
(define (disj-left f)
  (second f))
(define (disj-right f)
  (third f))
(define (conj-left f)
  (second f))
(define (conj-right f)
  (third f))

(define (free-vars xs)
  (define (find-vars h acc)
    (if (null? h)
        acc
        (cond
          [(var? h) (cons h acc)]
          [(neg? h)
           (find-vars (neg-subf h) acc)]
          [(disj? h)
           (find-vars (disj-left h)
                      (find-vars (disj-right h) acc))]
          [(conj? h)
           (find-vars (conj-left h)
                      (find-vars (conj-right h) acc))])))
  (find-vars xs null))


(free-vars (make-neg (make-conj (make-neg 't) (make-disj (make-conj (make-neg 'w) (make-disj 's 'u)) 'q))))
(free-vars 'p)

(define (gen-vals xs)
  (if (null? xs)
      (list null)
      (let*
          ((vss (gen-vals (cdr xs)))
           (x (car xs))
           (vst (map (lambda (vs) (cons (list x true ) vs)) vss))
           (vsf (map (lambda (vs) (cons (list x false) vs)) vss)))
        (append vst vsf))))
