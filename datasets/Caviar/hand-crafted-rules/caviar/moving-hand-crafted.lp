%*
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
*%

%*
terminatedAt(moving(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      orientationFar(X1,X0,X2),
      close(X1,X0,25,X2).

terminatedAt(moving(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      far(X1,X0,25,X2),
      far(X1,X0,24,X2),
      happensAt(disappear(X0),X2).

terminatedAt(moving(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(disappear(X0),X2).

terminatedAt(moving(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(disappear(X1),X2).

terminatedAt(moving(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(active(X0),X2).

terminatedAt(moving(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(active(X1),X2).

initiatedAt(moving(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      close(X0,X1,24,X2),
      happensAt(walking(X1),X2),
      orientationMove(X1,X0,X2),
      happensAt(walking(X0),X2),
      not happensAt(disappear(X0),X2),
      not happensAt(disappear(X1),X2).
*%


