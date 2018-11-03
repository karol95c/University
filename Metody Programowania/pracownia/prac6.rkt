#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (hole? t)
  (eq? t 'hole))
 
(define empty-set
  (identity null))
 
(define (add-to-set x s)
  (cond [(null? s) (list x)]
        [(eq? x (car s)) s]
        [else (cons (car s) (add-to-set x (cdr s)))]))
 
(define (merge-sets s1 s2)
  (cond [(null? s1) s2]
        [else (add-to-set (car s1) (merge-sets (cdr s1) s2))]))


(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (member-var var xs)
  (if (not (member-var var xs))
      false
      true))

(define (hole-context e)
  (define (scan-for-holes exp res)
    (cond [(hole? exp) res]
          [(const? exp) empty-set]
          [(binop? exp)
           ;(if (> (num-of-holes (binop-left exp)) 0)
               (scan-for-holes (binop-left exp) res)
               (scan-for-holes (binop-right exp) res)]
          [(let? exp)
           (if (not (hole? (let-def-expr exp)))
               (merge-sets (scan-for-holes (let-def-expr (let-def exp)) res)
                           (scan-for-holes
                            (let-expr exp) (add-to-set (let-def-var (let-def exp))
                                                       res)))
               res)]
            [(var? exp) null]))
  (if (arith/let/holes? e)
      (scan-for-holes e null)
      (error "Wyrazenie nie posiada 'dziury'!")))

(define (test)
  (define (members xs ys)
    (cond [(null? xs) true]
          [(not (member (car xs) ys)) false]
          [else (members (cdr xs) ys)]))
  (define (tester vars exp)
    (let ((res (hole-context exp)))
      (and (equal? (length vars) (length res))
           (members res vars))))
  (print (tester '(y) '(let (y 5) (+ 5 hole))))
  (newline)
  (print (tester '(a b c d e f g) '(let (a 1) (let (b 2) (let (c 3) (let (d 4)
                                (let (e 5) (let (f 6) (let (g 7) (+ 8 hole))))))))))
  (newline)
  (print (tester '(z) '(+ (let (x 4) (+ x 5)) (let (z 5) hole))))
  (newline)
  (print (tester '(x z) '(let (x 25) (+ (let (y 100) (* x y)) (let (z 5) hole)))))
  (newline)
  (print (tester '(z b a) '(let (a 10) (let (b 20) (+ (let (y 10) (let (x 20) (+ x y))) (let (z 5) hole)))))))


(test)