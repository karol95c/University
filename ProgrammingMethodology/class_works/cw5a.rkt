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



(define (gen-vals xs)
  (if (null? xs)
      (list null)
      (let*
          ((vss (gen-vals (cdr xs)))
           (x (car xs))
           (vst (map (lambda (vs) (cons (list x true ) vs)) vss))
           (vsf (map (lambda (vs) (cons (list x false) vs)) vss)))
        (append vst vsf))))

(define (nnf? f)
  (define (literal? f)
    (or (var? f)
        (and (neg? f) 
             (var? (neg-subf f)))))
  (or (literal? f)
      (and (disj? f)
           (nnf? (disj-left f))
           (nnf? (disj-right f)))
      (and (conj? f)
           (nnf? (conj-left f))
           (nnf? (conj-right f)))))

(define (convert-to-nnf f)
    (define (convert-neg f)
      (cond [(var? f) (make-neg f)]
            [(neg? f) (convert-to-nnf (neg-subf f))]
            [(conj? f) (make-conj (convert-to-nnf (make-neg (conj-left f)))
                                  (convert-to-nnf (make-neg (conj-right f))))]
            [(disj? f) (make-disj (convert-to-nnf (make-neg (disj-left f)))
                                  (convert-to-nnf (make-neg (disj-right f))))]))
      
  (cond [(var? f) f]
        [(conj? f) (make-conj (convert-to-nnf (conj-left f))
                              (convert-to-nnf (conj-right f)))]
        [(disj? f) (make-disj (convert-to-nnf (disj-left f))
                              (convert-to-nnf (disj-right f)))]
        [(neg? f) (convert-neg (convert-to-nnf (neg-subf f)))]))

(define (eval-formula f val)
  (define (get-value z v)
    (if (null? v)
        (error "Zmienna niezdefiniowana w wartosciowaniu.")
        (let ((x (car v)))
          (if (eq? z (car x))
              (cadr x)
              (get-value z (cdr v))))))
  (cond [(var? f) (get-value f val)]
        [(neg? f) (not (eval-formula (neg-subf f) val))]
        [(conj? f) (and (eval-formula (conj-left f) val)) (eval-formula (conj-right f) val)]
        [(disj? f) (or (eval-formula (disj-left f) val)) (eval-formula (disj-right f) val)]))
        

(define (falsifiable f)
  (define (gen-vals xs)
  (if (null? xs)
      (list null)
      (let*
          ((vss (gen-vals (cdr xs)))
           (x (car xs))
           (vst (map (lambda (vs) (cons (list x true ) vs)) vss))
           (vsf (map (lambda (vs) (cons (list x false) vs)) vss)))
        (append vst vsf))))
  (define (eval-formula f val)
  (define (get-value z v)
    (if (null? v)
        (error "Zmienna niezdefiniowana w wartosciowaniu.")
        (let ((x (car v)))
          (if (eq? z (car x))
              (cadr x)
              (get-value z (cdr v))))))
  (cond [(var? f) (get-value f val)]
        [(neg? f) (not (eval-formula (neg-subf f) val))]
        [(conj? f) (and (eval-formula (conj-left f) val)) (eval-formula (conj-right f) val)]
        [(disj? f) (or (eval-formula (disj-left f) val)) (eval-formula (disj-right f) val)]))
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
  
  (define (helper t val)
    (if (null? val)
        false
        (if (not (eval-formula t (car val)))
            (car val)
            (helper t (cdr val)))))
  (let* ((variables (free-vars f))
        (values (gen-vals variables)))
    (helper f values)))
    
  
;(gen-vals '(p q r))

;(free-vars (make-neg (make-conj (make-neg 't) (make-disj (make-conj (make-neg 'w) (make-disj 's 'u)) 'q))))
;(free-vars 'p)
(define formula (make-conj (make-conj 'p 'q) (make-neg 'r)))
(define formula-values '((p #t) (q #t)))
(falsifiable formula)
;(convert-to-nnf (make-neg (make-conj (make-neg 't) (make-disj (make-conj (make-neg 'w) (make-disj 's 'u)) 'q))))
