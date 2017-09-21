initiatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(walking(X), T),
  happensAt(walking(Y), T),
  not happensAt(disappear(X), T),  % needed to generate clean data
  not happensAt(disappear(Y), T),  % needed to generate clean data
  orientationMove(X, Y, T),
  close(X, Y, 24, T).

initiatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(walking(X), T),
  happensAt(walking(Y), T),
  holdsAt(visible(X), T),
  holdsAt(visible(Y), T),
  orientationMove(X, Y, T),
  close(X, Y, 24, T).

%terminatedAt(moving(X, Y), T) :-
%  person(X),person(Y),time(T),
%  far(X, Y, 34, T).

%terminatedAt(moving(X0,X1),X2) :-
%  person(X0),person(X1),time(X2),
%  orientationFar(X1,X0,X2).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(active(X), T),
  happensAt(active(Y), T).

terminatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(inactive(X), T),
  happensAt(inactive(Y), T).

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



%*

%% For generating clean data?

initiatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(walking(X), T),
  happensAt(walking(Y), T),
  not happensAt(disappear(X), T),  % needed to generate clean data
  not happensAt(disappear(Y), T),  % needed to generate clean data
  orientationMove(X, Y, T),
  close(X, Y, 34, T).

initiatedAt(moving(X, Y), T) :-
  person(X),person(Y),time(T),
  happensAt(walking(X), T),
  happensAt(walking(Y), T),
  holdsAt(visible(X), T),  % needed to generate clean data
  holdsAt(visible(Y), T),  % needed to generate clean data
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

