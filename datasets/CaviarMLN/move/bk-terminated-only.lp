
% The initiation axiom is needed here to ensure that we won't an UNSAT
% error in case a fluent is initiated in the middle of a training sequence.
% So we abduce initiatedAt atoms as well, but we filter them out before
% kernel set construction (with the learningTerminatedOnly parameter).

holdsAt(F,Te) :- fluent(F), initiatedAt(F,Ts), Te = Ts + 1, time(Ts), time(Te).

holdsAt(F,Te) :- fluent(F), holdsAt(F,Ts), not terminatedAt(F,Ts), Te = Ts + 1, time(Ts), time(Te).

holdsAt(F,T) :- initialTime(T), example(holdsAt(F,T)).

initialTime(X) :- time(X), #false : X > Y, time(Y).


% the overall initial time, extracted by the rule
% initialTime(X) :- time(X), #false : X > Y, time(Y).
% may differ from the time of the first observed example.
% Since we do not have an axiom to initiate the corresponding fluent
% here (we are learning terminatedAt only), we need to add an additional rule
% that extract the examples' initial time:
% examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).
% and an additional axiom as below:

holdsAt(F,T) :- fluent(F), examplesInitialTime(T), example(holdsAt(F,T)).

examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).

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


