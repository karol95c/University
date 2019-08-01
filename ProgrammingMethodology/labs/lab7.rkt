#lang racket

;; expressions

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * /))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op-cons op args)
  (cons op args))

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

(define (arith/let-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith/let-expr? (op-args t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def-expr (let-def t))))
      (var? t)))

;; let-lifted expressions

(define (arith-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith-expr? (op-args t)))
      (var? t)))

(define (let-lifted-expr? t)
  (or (and (let? t)
           (let-lifted-expr? (let-expr t))
           (arith-expr? (let-def-expr (let-def t))))
      (arith-expr? t)))

;; generating a symbol using a counter

(define (number->symbol i)
  (string->symbol (string-append "x" (number->string i))))

;; environments (could be useful for something)

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; the let-lift procedure

(define (inc-counter)
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))

(define (lift-res t)
  (car t))
(define (lift-res-env t)
  (caar t))

(define (lift-res-arith t)
 (cdar t))

(define (lift-counter t)
  (cdr t))

(define (let-lift e)
  (define (lifting exp i env)
    (cond [(const? exp) (cons (cons env exp) i)]
          [(var? exp) (cons (cons env exp) i)]
          [(op? exp)
           (let* ([r1 (lifting (first (op-args exp)) i null)]
                  [r2 (lifting (second (op-args exp)) (lift-counter r1) null)])
             (cons (cons (append (lift-res-env r1) (lift-res-env r2))
                   (list (op-op exp) (lift-res-arith r1) (lift-res-arith r2))) (lift-counter r2)))]
          [(let? exp)
           (let* ([l1 (lifting (let-def-expr (let-def exp))
                              (+ i 1) env)]
                  [l2 (lifting (let-expr exp) (lift-counter l1)
                              (add-to-env (number->symbol i) (lift-res-arith l1) (lift-res-env l1)))])
                  
             (cons (cons (lift-res-env l2)
                   (list (op-op exp) (lift-res-arith l1) (lift-res-arith l2))) (lift-counter l2)))]))
                 
    (lifting e 0 empty-env))
                 
           
(define p1 (cons (cons 1 2) 3))
(define p2 (list (cons 1 2) 3))
(define a (let-lift '( let (x (- 2 ( let ( z 3) z ) ) ) (+ x 2) )))
(let-lift '(+ 10 (* ( let ( x 7) (+ x 2) ) 2) ))


  