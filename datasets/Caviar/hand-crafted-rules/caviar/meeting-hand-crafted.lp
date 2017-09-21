initiatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(active(X), T),
   not happensAt(disappear(X), T), % this is needed to generate consistent clear data, but it is not part of the original hand-crafted rules
   not happensAt(disappear(Y), T), % this is needed to generate consistent clear data, but it is not part of the original hand-crafted rules
   not happensAt(running(Y), T),
   close(X, Y, 25, T).

% Dual of the above. This is needed to generate
% correct synthetic but it is not part of the
% original hand-crafted rules.
initiatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(active(Y), T),
   not happensAt(disappear(Y), T), % this is needed to generate consistent clear data, but it is not part of the original hand-crafted rules
   not happensAt(disappear(X), T), % this is needed to generate consistent clear data, but it is not part of the original hand-crafted rules
   not happensAt(running(X), T),
   close(X, Y, 25, T).

initiatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(inactive(X), T),
   not happensAt(disappear(X), T), % this is needed to generate clear data, but it is not part of the original hand-crafted rules
   not happensAt(disappear(Y), T), % this is needed to generate consistent clear data, but it is not part of the original hand-crafted rules
   not happensAt(running(Y), T),
   not happensAt(active(Y), T),
   close(X, Y, 25, T).

% Dual of the above. This is needed to generate
% correct synthetic but it is not part of the
% original hand-crafted rules.
initiatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(inactive(Y), T),
   not happensAt(disappear(Y), T), % this is needed to generate clear data, but it is not part of the original hand-crafted rules
   not happensAt(disappear(X), T), % this is needed to generate consistent clear data, but it is not part of the original hand-crafted rules
   not happensAt(running(X), T),
   not happensAt(active(X), T),
   close(X, Y, 25, T).

terminatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(running(X), T).

terminatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(running(Y), T).

terminatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(disappear(X), T).

terminatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(disappear(Y), T).

terminatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(walking(X), T),
   far(X, Y, 34, T).

terminatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(walking(Y), T),
   far(X, Y, 34, T).




%*
terminatedAt(meeting(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(walking(X0),X2),
      happensAt(inactive(X1),X2).

terminatedAt(meeting(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(walking(X1),X2),
      happensAt(inactive(X0),X2).

terminatedAt(meeting(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(running(X1),X2),
      close(X1,X0,25,X2).

terminatedAt(meeting(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      happensAt(running(X0),X2),
      close(X1,X0,25,X2).

initiatedAt(meeting(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      close(X1,X0,25,X2),
      happensAt(active(X0),X2),
      happensAt(active(X1),X2),
      holdsAt(visible(X0),X2).

initiatedAt(meeting(X0,X1),X2) :-
      person(X0),person(X1),time(X2),
      close(X1,X0,25,X2),
      happensAt(inactive(X1),X2),
      happensAt(inactive(X0),X2).
*%

