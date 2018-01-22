
initiatedAt(F,Ts) :- example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

false_initiatedAt(F,Ts) :- not example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

terminatedAt(F,Ts) :- example(holdsAt(F,Ts)), not example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

% No termination if we have initiation at the next time point.
false_terminatedAt(F,Ts) :- example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

% No termination if we have no holdsAt (no need).
false_terminatedAt(F,Ts) :- not example(holdsAt(F,Ts)), fluent(F), time(Ts).

fluent(moving(X,Y)) :- person(X),person(Y),X != Y.
fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.

event(E) :- happensAt(E,_).
time(T):- example(holdsAt(_,T)).
time(X):- happensAt(walking(_),X).
time(X):- happensAt(active(_),X).
time(X):- happensAt(inactive(_),X).
time(X):- happensAt(abrupt(_),X).
time(X):- happensAt(running(_),X).
time(X):- happensAt(appear(_),X).
time(X):- happensAt(disappear(_),X).
time(X):- orientation(_,_,X).
time(X):- coords(_,_,_,X).

next(X, Z) :- time(X), time(Z), X > Z, #false : Z < Y, Y < X, time(Y).

person(id1). person(id2).
time(1..10).
happensAt(walking(id1),3). happensAt(active(id2),4).
coords(id1, 10, 50, 3). coords(id2, 10, 50, 3).
example(holdsAt(meeting(id1,id2),4)). example(holdsAt(meeting(id2,id1),4)).
example(holdsAt(meeting(id1,id2),5)). example(holdsAt(meeting(id2,id1),5)).
example(holdsAt(meeting(id1,id2),6)). example(holdsAt(meeting(id2,id1),6)).
example(holdsAt(meeting(id1,id2),7)). example(holdsAt(meeting(id2,id1),7)).

#include "arithmetic.lp".

#show.
#show initiatedAt/2.
#show false_initiatedAt/2.
#show initiatedAt/2.
#show terminatedAt/2.
#show false_terminatedAt/2.
#show happensAt/2.
#show event/1.
#show fluent/1.
#show close/4.
#show far/4.
#show orientationMove/3.
#show orientationFar/3.
#show next/2.