number(1..100).

less_than(X,Y) :- X < Y, number(X), number(Y).
less_than(X,Y) :- less_than(X,Z), number(Z), less_than(Z,Y).

#show.
#show less_than/2.