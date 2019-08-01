#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości

(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;;
;; WHILE
;;

; memory

(define empty-mem
  null)

(define (set-mem x v m)
  (cond [(null? m)
         (list (cons x v))]
        [(eq? x (caar m))
         (cons (cons x v) (cdr m))]
        [else
         (cons (car m) (set-mem x v (cdr m)))]))

(define (get-mem x m)
  (cond [(null? m) 0]
        [(eq? x (caar m)) (cdar m)]
        [else (get-mem x (cdr m))]))

; arith and bool expressions: syntax and semantics

(define (const? t)
  (number? t))

(define (true? t)
  (eq? t 'true))

(define (false? t)
  (eq? t 'false))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <= not and or mod rand pow))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=) =]
        [(eq? op '>) >]
        [(eq? op '>=) >=]
        [(eq? op '<)  <]
        [(eq? op '<=) <=]
        [(eq? op 'not) not]
        [(eq? op 'and) (lambda x (andmap identity x))]
        [(eq? op 'or) (lambda x (ormap identity x))]
        [(eq? op 'mod) (lambda (a b) (modulo a b))]
        [(eq? op 'pow) (lambda (a b) (expt a b))]
        [(eq? op 'rand) (lambda (max) (min max 4))]))
        ;[(eq? op 'rand) (lambda (max) ((rand max) initial-seed))]))

(define (var? t)
  (symbol? t))

(define (eval-arith e m)
  (cond [(true? e) true]
        [(false? e) false]
        [(var? e) (get-mem e m)]
        [(op? e)
         (apply
          (op->proc (op-op e))
          (map (lambda (x) (eval-arith x m))
               (op-args e)))]
        [(const? e) e]))


;; syntax of commands

(define (assign? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (second t) ':=)))

(define (assign-var e)
  (first e))

(define (assign-expr e)
  (third e))

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

(define (while? t)
  (tagged-tuple? 'while 3 t))

(define (while-cond t)
  (second t))

(define (while-expr t)
  (third t))

(define (block? t)
  (list? t))

;; state

(define (res v s)
  (cons v s))

(define (res-val r)
  (car r))

(define (res-state r)
  (cdr r))

;; psedo-random generator

(define initial-seed
  123456789)

(define (rand max)
  (lambda (i)
    (let ([v (modulo (+ (* 1103515245 i) 12345) (expt 2 32))])
      (res (modulo v max) v))))

;; WHILE interpreter

(define (old-eval e m)
  (cond [(assign? e)
         (set-mem
          (assign-var e)
          (eval-arith (assign-expr e) m)
          m)]
        [(if? e)
         (if (eval-arith (if-cond e) m)
             (old-eval (if-then e) m)
             (old-eval (if-else e) m))]
        [(while? e)
         (if (eval-arith (while-cond e) m)
             (old-eval e (old-eval (while-expr e) m))
             m)]
        [(block? e)
         (if (null? e)
             m
             (old-eval (cdr e) (old-eval (car e) m)))]))

(define (eval e m seed)
  (define (evaluation e m seed)
  ;; TODO : ZAD B: Zaimplementuj procedurę eval tak, by
  ;;        działała sensownie dla wyrażeń używających
  ;;        konstrukcji "rand".
   (cond [(assign? e)
          (let ((r (eval-arith-seed (assign-expr e) m seed)))
            (res (set-mem
                  (assign-var e)
                  (res-val r)
                  m)
                 (res-state r)))]
        [(if? e)
         (let ((r (eval-arith-seed (if-cond e) m seed)))
               (if (res-val r)
                   (evaluation (if-then e) m (res-state r))
                   (evaluation (if-else e) m (res-state r))))]
        [(while? e)
         (let ((r (eval-arith-seed (while-cond e) m seed)))
           (if (res-val r)
               (let ((r1 (evaluation (while-expr e) m (res-state r))))
                 (evaluation e (res-val r1) (res-state r1)))
               (res m seed)))]
        [(block? e)
         (if (null? e)
             (res m seed)
             (let ((r (evaluation (car e) m seed)))
               (evaluation (cdr e) (res-val r) (res-state r))))]))
  (eval-memory (evaluation e m seed)))

(define (run e)
  (eval e empty-mem initial-seed))

;;

(define (probably-prime? n k)
  ;( ; check if a number n is prime using
                              ; k iterations of Fermat's primality
                              ; test
  (let ([memory (set-mem 'k k
                (set-mem 'n n empty-mem))])
    (not (get-mem
           'composite
           (eval fermat-test memory initial-seed)))))
           ;(old-eval fermat-test memory)))))

(define (rand? t)
  (tagged-tuple? 'rand 2 t))

(define (rand-max t)
  (second t))


(define (eval-memory t)
  (car t))

(define (eval-arith-seed e m s)
  (define (eval-apply xs st acc)
    (if (null? xs)
        (res (reverse acc) st)
        (let ((result (eval-arith-seed (car xs) m st)))
          (eval-apply (cdr xs) (res-state result) (cons (res-val result) acc)))))                                  
  (cond [(true? e) (res true s) ]
        [(false? e) (res false s)]
        [(var? e) (res (get-mem e m) s)]
        [(rand? e) 
         (let ((result (eval-arith-seed (rand-max e) m s)))
           ((rand (res-val result)) (res-state result)))]
        [(op? e)
         (let ((r (eval-apply (op-args e) s null)))
           (res (apply
                 (op->proc (op-op e))
                 (res-val r))
                (res-state r)))]
        [(const? e) (res e s)]))

(define fermat-test
  '( ( i := 0 )
     ( composite := false )
     ( while ( < i k)
             (( a := (+ 2 (rand (- n 4)) ))
              ( if ( = (mod (pow a (- n 1)) n) 1 )
                   ( i := (+ i 1) )
                   ( ( composite := true )
                     ( i := k )))))))

;;wlasne funkcje do testowania

(define (seed-update)
    (let ([n initial-seed])
      (lambda ()
        (set! n (+ n 1))
        n)))
  (define seed-up (seed-update))

(define test-rand
  '(
     ( x := (rand 100))
     ( y := (rand 100))
     ( a := (+ 2 (rand (- n 4))))
     ( b := (pow a (- 12 1)))))
(define test1
  '(( a := (mod 5 20))))


;;liczby złożone
(display "Test dla liczb złożonych\n")
(probably-prime? 8 2)
(probably-prime? 15 2)
(probably-prime? 27 1)
(probably-prime? 204 2)
(probably-prime? 357 2)
(probably-prime? 1024 2)
(probably-prime? 1000214 3)
(probably-prime? 2004221 10)
(probably-prime? 5415215 10)

;;liczby pierwsze
(display "Testy dla liczb pierwszych\n")
(probably-prime? 13 5)
(probably-prime? 23 5)
(probably-prime? 653 10)
(probably-prime? 1451 30)
(probably-prime? 2309 30)
(probably-prime? 12653 30)
(probably-prime? 13477 30)
(probably-prime? 16319 30)
(probably-prime? 17389  30)
(probably-prime? 522191 100)
