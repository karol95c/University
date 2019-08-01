#lang racket
 
(require racklog)
 
;; predykat unarny %male reprezentuje zbiór mężczyzn
(define %male
  (%rel ()
        [('adam)]
        [('john)]
        [('joshua)]
        [('mark)]
        [('david)]))
 
;; predykat unarny %female reprezentuje zbiór kobiet
(define %female
  (%rel ()
        [('eve)]
        [('helen)]
        [('ivonne)]
        [('anna)]))
 
;; predykat binarny %parent reprezentuje relację bycia rodzicem
(define %parent
  (%rel ()
        [('adam 'helen)]
        [('adam 'ivonne)]
        [('adam 'anna)]
        [('eve 'helen)]
        [('eve 'ivonne)]
        [('eve 'anna)]
        [('john 'joshua)]
        [('helen 'joshua)]
        [('ivonne 'david)]
        [('mark 'david)]))
 
;; predykat binarny %sibling reprezentuje relację bycia rodzeństwem
(define %sibling
  (%rel (a b c)
        [(a b)
         (%parent c a)
         (%parent c b)]))
 
;; predykat binarny %sister reprezentuje relację bycia siostrą
(define %sister
  (%rel (a b)
        [(a b)
         (%sibling a b)
         (%female a)]))
 
;; predykat binarny %ancestor reprezentuje relację bycia przodkiem
(define %ancestor
  (%rel (a b c)
        [(a b)
         (%parent a b)]
        [(a b)
         (%parent a c)
         (%ancestor c b)]))
(define %grandson
  (%rel (x y z)
        [(x y)
         (%parent z x)
         (%parent y z)]))

(define %cousin
  (%rel (x y z w)
        [(x y w)
         (%parent z y)
         (%parent w x)
         (%sibling w z)]))

(define %cousin1
  (%rel (a b c d)
        [(a b)
         (%sibling c d)
         (%parent c a)
         (%parent d b)]))

(define %is_mother
  (%rel (a)
        [(a)
         (%female a)
         (%parent a (_))]))
(define %is_father
  (%rel (a)
        [(a)
         (%male a)
         (%parent a (_) )]))
 
(define %my-append
  (%rel (x xs ys zs)
        [(null ys ys)]
        [((cons x xs) ys (cons x zs))
         (%my-append xs ys zs)]))
 
(define %my-member
  (%rel (x xs y)
        [(x (cons x xs))]
        [(y (cons x xs))
         (%my-member y xs)]))
 
(define %select
  (%rel (x xs y ys)
        [(x (cons x xs) xs)]
        [(y (cons x xs) (cons x ys))
         (%select y xs ys)]))
 
;; prosta rekurencyjna definicja
(define %simple-length
  (%rel (x xs n m)
        [(null 0)]
        [((cons x xs) n)
         (%simple-length xs m)
         (%is n (+ m 1))]))


(define %prefix
  (%rel (x xs ys)
        [(null ys)]
        [((cons x xs) (cons x ys))
         (%prefix xs ys)]))

(define %suffix
  (%rel (xs y ys)
        [(xs ys)]
        [(xs (cons y ys))
         (%suffix xs ys)]))
         
;(define %sublist
;  (%rel (x xs ys)
;        [(null ys)]
;        [(xs ys)]
;        [(xs (cons x ys))
;         (%sublist xs ys)]))

(define %sublist
  (%rel (xs ys x y)
        [(null ys)]
        ; (%my-append null xs ys)
        [((cons x xs) (cons x ys))
         (%sublist xs ys)]
        [((cons x xs) (cons y ys))!
         (%sublist (cons x xs) ys)]
        [(xs ys)
         (%/= xs null)
         (%= xs ys)]))

(define %reverse
  (%rel (xs ys x acc)
        [(xs ys)
         (%reverse xs null ys)]
        [((cons x xs) acc ys)
         (%reverse xs (cons x acc) ys)]
        [(null ys ys)]))


;; test w trybie +- (działa)
;(%find-all (a) (%simple-length (list 1 2) a))
;; test w trybie ++ (działa)
;(%find-all () (%simple-length (list 1 2) 2))
;; test w trybie -+ (1 odpowiedź, pętli się)
;(%which (xs) (%simple-length xs 2))
;; test w trybie -- (nieskończona liczba odpowiedzi)
;(%which (xs a) (%simple-length xs a))
 
;; definicja zakładająca, że długość jest znana
(define %gen-length
  (%rel (x xs n m)
        [(null 0) !]
        [((cons x xs) n)
         (%is m (- n 1))
         (%gen-length xs m)]))
;; test w trybie ++ (działa)
;(%find-all () (%gen-length (list 1 2) 2))
;; test w trybie -+ (działa)
;(%find-all (xs) (%gen-length xs 2))
 
 
;(%find-all (x) (%ancestor  'mark 'john))
;(%find-all (x) (%ancestor 'adam x))
;(%find-all (x) (%sister 'ivonne x))
;(%find-all (x) (%cousin1 'joshua x))
;(%find-all (x) (%my-append  (list 1 2 3 4 5) x))

;;cw3
;(%which (x y) (%append x x y))
;(%which (x) (%select x '(1 2 3 4) '(1 2 3)))
;(%which (x) (%select 'a x '(b c d)))
;(%which (x) (%append '(1 2 3) x '(1 2 3 4 5)))
;(%find-all (x) (%sublist x '(1 2 3 4)))
;(%find-all (x) (%prefix x '(1 2 3 4)))

;(%find-all (x) (%suffix x '(1 2 3 4)))
;(%which () (%prefix '(1 2 3) '(1 2 3 4)))
;(%which () (%prefix '(1 25) '(1 2 3 4)))
;(%which () (%suffix '(3 4) '(1 2 3 4)))
(%which () (%suffix '(2 5 3) '(1 2 4)))
;(%which (x) (%reverse '(1 2 3) x))

