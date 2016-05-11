
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














