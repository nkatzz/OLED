terminatedAt(meeting(X0,X1),X2) :-
      happensAt(disappear(X0),X2),
      person(X1).

%terminatedAt(meeting(X0,X1),X2) :-
%      happensAt(active(X0),X2),
%      happensAt(active(X1),X2),
%      close(X1,X0,25,X2),
%      close(X0,X1,25,X2).

terminatedAt(meeting(X0,X1),X2) :-
      happensAt(running(X1),X2),
      close(X1,X0,25,X2).

terminatedAt(meeting(X0,X1),X2) :-
      happensAt(running(X0),X2),
      close(X1,X0,25,X2).

terminatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      close(X1,X0,25,X2).

terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      close(X1,X0,25,X2).

%initiatedAt(meeting(X0,X1),X2) :-
%      close(X1,X0,25,X2).

initiatedAt(meeting(X0,X1),X2) :-
      close(X0,X1,34,X2),
      happensAt(active(X0),X2),
      happensAt(active(X1),X2).

initiatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,40,X2),
      happensAt(active(X0),X2),
      happensAt(active(X1),X2).