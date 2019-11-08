#lang datalog

;;edge(a, b). edge(b, c). edge(c, d). edge(d, a). edge(d, f). edge(a, d). edge(b, f). edge(f, e).
;;edge(n, f). edge(n, g). edge(m, h). edge(m, g).
edge(a, b).
edge(a, c).
edge(a, d).
edge(c, d).
edge(b, c).

edge(a, b, 1).path(X, Y) :- edge(X, Y), lower(2, 1).
path(X, Y) :- path(X, Z), path(Z, Y).
cycle(X) :- path(X, X).
goAndBack(X, Y):- path(X, Y), path(Y, X).
isPath(X) :- path(n, X).
isPath(X) :- path(m, X).

odd(X, Y) :- edge(X, Y).
odd(X, Y) :- edge(X, Z), even(Z, Y).
even(X, Y) :- edge(X, Z), odd(Z, Y).
q() :- odd(X, X).
path(X, Y, N) :- edge(X, 1).
;;q()?
lower(X, Y) :- X = Y.
;;cycle(X)?
;;goAndBack(X, Y)?
;;isPath(X)?


