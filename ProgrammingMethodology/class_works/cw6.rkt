#lang racket


; To, jakie konstrukcje występują w języku i jakich
; instrukcji i/lub wyrażeń potrzebują, nazywamy składnią abstrakcyjną.
; Natomiast reguły ich zapisywania w postaci tekstu to składnia konkretna.
; Parsowanie programu to przekształcenie składni konkretnej na składnie abstrakcyjną
; Homoikoniczność to cecha języków programowania, w których reprezentacja programu
; jest jednocześnie podstawową strukturą danych wykorzystywaną w języku
; Tekst języka jest ma taką samą budowe jak abstrakcyjne drzewo składni programu
; Wewnetrzna reprezentacja programu może być oczytana z układu tekstu programu




;; arithmetic expressions

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

(define (arith-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith-expr? (binop-left  t))
           (arith-expr? (binop-right t)))))

(define (if-zero? t)
  (and (list? t)
       (= (length t) 4)
       (eq? 'if-zero (car t))))

(define (if-zero-cons e l r)
  (list 'if-zero e l r))

(define (if-zero-exp t)
  (cadr t))

(define (if-zero-left t)
  (caddr t))

(define (if-zero-right t)
  (cadddr t))



;; calculator

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (eval-arith e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-arith (binop-left  e))
            (eval-arith (binop-right e)))]))

;; let expressions

(define (let-db? t)
   (and (list? t)
         (= (length t) 3)
         (eq? (car t) 'let)))
(define (let-db-def e)
   (cadr e))
(define (let-db-expr e)
   (caddr e))
(define (db-index? e)
   (and (list? e)
         (= (length e) 2)
         (eq? (car e) 'index)
         (number? (cadr e))))

;(define (arith/let-expr-db? t)
 ; (or (self-evaluating? t)
  ;    (and (binop? t)
    ;       (arith/let-expr-db?
   ;         (arith/let-expr-db?
     ;        (and (expr-let-db? t)
      ;            (arith/let-expr-db?
       ;            (arith/let-expr-db?
        ;            (db-index? t)))
         ;         (binop-left t))
         ;    (binop-right t)))
          ; (let-db-def t))
      ;(let-db-expr t)))

;(define (tr-db env e)
 ; (cond [(const? e) e]
  ;      [(binop? e)
   ;      (binop-cons (binop-op e)
    ;                 (tr-db (binop-left e) env)
     ;                (tr-db (binop-right e)) env)]
      ;  [(var? e) (db-index-cons (get-index env e))]
        ;[(let? e) (db-let-cons (tr-db env (let-db e))
       ;                        (add-to-env 


                               
        
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
      (and (binop? t)
           (arith/let-expr? (binop-left  t))
           (arith/let-expr? (binop-right t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def (let-def-expr t))))
      (var? t)))

;; evalation via substitution

(define (subst e x f)
  (cond [(const? e) e]
        [(binop? e)
         (binop-cons
           (binop-op e)
           (subst (binop-left  e) x f)
           (subst (binop-right e) x f))]
        [(let? e)
         (let-cons
           (let-def-cons
             (let-def-var (let-def e))
             (subst (let-def-expr (let-def e)) x f))
           (if (eq? x (let-def-var (let-def e)))
               (let-expr e)
               (subst (let-expr e) x f)))]
        [(var? e)
         (if (eq? x (var-var e))
             f
             (var-var e))]
        [(if-zero? e)
           (if-zero-cons (subst (if-zero-exp e) x f)
                         (subst (if-zero-left e) x f)
                         (subst (if-zero-right e) x f))]))
            

(define (eval-subst e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-subst (binop-left  e))
            (eval-subst (binop-right e)))]
        [(let? e)
         (eval-subst
           (subst
             (let-expr e)
             (let-def-var (let-def e))
             (eval-subst (let-def-expr (let-def e)))))]
        [(if-zero? e)
         (if (= (eval-subst (if-zero-exp e)) 0)
             (eval-subst (if-zero-left e))
             (eval-subst (if-zero-right e)))]
        [(var? e)
         (error "undefined variable" (var-var e))]))

;; evaluation via environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

(define (eval-env e env)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-env (binop-left  e) env)
            (eval-env (binop-right e) env))]
        [(let? e)
         (eval-env
           (let-expr e)
           (env-for-let (let-def e) env))]
        [(if-zero? e)
 *         (if (= (eval-env (if-zero-exp e) env) 0)
             (eval-env (if-zero-left e) env)
             (eval-env (if-zero-right e) env))]
        [(var? e) (find-in-env (var-var e) env)]))



(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

(define (eval e)
  (eval-env e empty-env))

(define (arith->onp e)
  (define (iter expr acc)
    (cond [(const? expr) (cons expr acc)]
          [(binop? expr)
           (iter (binop-left expr) (iter (binop-right expr) (cons (binop-op expr) acc)))]))
  (iter e null))


/(define a (binop-cons '+ (binop-cons '/ 2 sda -cons '* 2 3 )))

(arith->onp a)

(eval '(if-zero (- 2 2) 7 9))
(eval '(if-zero 0 1 (/ 5 0)))

