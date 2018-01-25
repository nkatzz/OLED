
initiatedAt(F,Ts) :- example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

false_initiatedAt(F,Ts) :- not example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

terminatedAt(F,Ts) :- example(holdsAt(F,Ts)), not example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

% No termination if we have initiation at the next time point.
false_terminatedAt(F,Ts) :- example(holdsAt(F,Te)), fluent(F), time(Ts), time(Te), next(Te,Ts).

% No termination if we have no holdsAt (no need).
false_terminatedAt(F,Ts) :- not example(holdsAt(F,Ts)), fluent(F), time(Ts).

fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.

event(E) :- happensAt(E,_).
time(X):- example(holdsAt(_,X)).
time(X):- happensAt(walking(_),X).
time(X):- happensAt(active(_),X).
time(X):- happensAt(inactive(_),X).
time(X):- happensAt(abrupt(_),X).
time(X):- happensAt(running(_),X).
time(X):- happensAt(appear(_),X).
time(X):- happensAt(disappear(_),X).
time(X):- orientation(_,_,X).
time(X):- coords(_,_,_,X).

person(X):- happensAt(walking(X),_).
person(X):- happensAt(active(X),_).
person(X):- happensAt(inactive(X),_).
person(X):- happensAt(abrupt(X),_).
person(X):- happensAt(running(X),_).
person(X):- happensAt(appear(X),_).
person(X):- happensAt(disappear(X),_).

person(X) :- orientationMove(X,_,_).
person(X) :- orientationMove(_,X,_).
person(X) :- orientationFar(X,_,_).
person(X) :- orientationFar(_,X,_).
person(X) :- close(X,_,_,_).
person(X) :- close(_,X,_,_).
dist(X) :- close(_,_,X,_).
person(X) :- far(X,_,_,_).
person(X) :- far(_,X,_,_).
dist(X) :- far(_,_,X,_).

next(X, Z) :- time(X), time(Z), X > Z, #false : Z < Y, Y < X, time(Y).


happensAt(walking(id1),3). happensAt(active(id2),4). happensAt(walking(id3),5). happensAt(active(id4),6).
coords(id1, 10, 50, 3). coords(id2, 10, 50, 3).
holdsAt(meeting(id1,id2),4). holdsAt(meeting(id2,id1),4).
holdsAt(meeting(id1,id2),5). holdsAt(meeting(id2,id1),5).
holdsAt(meeting(id1,id2),6). holdsAt(meeting(id2,id1),6).
holdsAt(meeting(id1,id2),7). holdsAt(meeting(id2,id1),7).

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
#show person/1.
#show time/1.
#show dist/1.