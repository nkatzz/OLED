tps(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: example( holdsAt(meeting(X0,X1),Te) ), marked(I, initiatedAt(meeting(X0,X1),Ts) ), Te = Ts + 1, time(Te), time(Ts) }.
fps(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: not example( holdsAt(meeting(X0,X1),Te) ), marked(I, initiatedAt(meeting(X0,X1),Ts) ), Te = Ts + 1, time(Te), time(Ts) }.
fns(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: example( holdsAt(meeting(X0,X1),Te) ), not marked(I, initiatedAt(meeting(X0,X1),Ts) ), Te = Ts + 1, time(Te), time(Ts) }.

initialTime(X) :- time(X), #false : X > Y, time(Y).

fluent(moving(X,Y)) :- person(X),person(Y),X != Y.
fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.
fluent(fighting(X,Y)) :- person(X),person(Y),X != Y.
fluent(leavingObject(X,Y)) :- person(X),person(Y),X != Y.

far(X,Y,D,T) :- person(X), person(Y), X!=Y, time(T), dist(D), not close(X,Y,D,T), not close(Y,X,D,T).
orientationFar(X,Y,T) :- person(X), person(Y), X!=Y, time(T), not orientationMove(X,Y,T), not orientationMove(Y,X,T).

person(X):- happensAt(walking(X),_).
person(X):- happensAt(running(X),_).
person(X):- happensAt(active(X),_).
person(X):- happensAt(inactive(X),_).
person(X):- happensAt(abrupt(X),_).
person(X):- happensAt(appear(X),_).
person(X):- happensAt(disappear(X),_).
person(X):- orientationMove(X,_,_).
person(X):- orientationMove(_,X,_).
person(X):- close(X,_,_,_).
person(X):- close(_,X,_,_).

time(T):- example(holdsAt(_,T)).
time(X):- happensAt(walking(_),X).
time(X):- happensAt(active(_),X).
time(X):- happensAt(inactive(_),X).
time(X):- happensAt(abrupt(_),X).
time(X):- happensAt(running(_),X).
time(X):- happensAt(appear(_),X).
time(X):- happensAt(disappear(_),X).
time(X):- orientationMove(_,_,X).
time(X):- close(_,_,_,X).

dist(25).
dist(24).
dist(30).
dist(34).






%*
#script (python)

import math

def combinations(n, k):
  if (n >= k):
    return math.factorial(n)/( math.factorial(k) * math.factorial(n-k) )
  else:
    return 0

#end.

examplesCount(Count) :-
  Times = #count { Z: time(Z) },
  Persons = #count { P: person(P) },
  C = @combinations(Persons,2),
  C > 0,
  Count = Times * C.

examplesCount(Count) :-
  Times = #count { Z: time(Z) },
  Persons = #count { P: person(P) },
  C = @combinations(Persons,2),
  C = 0,
  Count = Times.
*%


