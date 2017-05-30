initiatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(walking(X), T),
  happensAt(walking(Y), T),
  not happensAt(disappear(X), T),  % needed to generate clean data
  not happensAt(disappear(Y), T),  % needed to generate clean data
  orientationMove(X, Y, T),
  close(X, Y, 34, T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(walking(X), T),
  far(X, Y, 34, T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(walking(Y), T),
  far(X, Y, 34, T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(active(X), T),
  happensAt(active(Y), T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(active(X), T),
  happensAt(inactive(Y), T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(inactive(X), T),
  happensAt(active(Y), T).


terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(running(X), T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(running(Y), T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(disappear(X), T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(disappear(Y), T).

