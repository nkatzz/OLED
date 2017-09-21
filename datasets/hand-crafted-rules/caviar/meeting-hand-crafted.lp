
initiatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(active(X), T),
   happensAt(active(Y), T),
   not happensAt(disappear(X), T),
   not happensAt(disappear(Y), T),
   close(X, Y, 25, T).

initiatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(active(X), T),
   happensAt(inactive(Y), T),
   not happensAt(disappear(X), T),
   not happensAt(disappear(Y), T),
   close(X, Y, 25, T).

initiatedAt(meeting(X, Y), T) :-
   person(X),person(Y),time(T),
   happensAt(inactive(X), T),
   happensAt(active(Y), T),
   not happensAt(disappear(X), T),
   not happensAt(disappear(Y), T),
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






%------------------------------------%
% fold_3
%--------
%*
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      happensAt(inactive(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      happensAt(walking(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      happensAt(walking(X0),X2).
initiatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,24,X2),
      happensAt(inactive(X0),X2),
      happensAt(walking(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      close(X1,X0,24,X2),
      happensAt(walking(X0),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      close(X0,X1,25,X2),
      happensAt(active(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      close(X0,X1,25,X2),
      happensAt(active(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      close(X0,X1,25,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      happensAt(inactive(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      close(X1,X0,24,X2).
initiatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,24,X2),
      happensAt(inactive(X0),X2).
*%
%-------------------------------------
%fold-4
%-------
%*
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      happensAt(walking(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      happensAt(inactive(X0),X2).
initiatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,24,X2),
      happensAt(inactive(X0),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      close(X1,X0,24,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      close(X0,X1,25,X2),
      happensAt(active(X1),X2).
*%
%--------------------------------------
%-------------------------------------
%fold-5
%-------
%*
% This is an XHAIL theory
terminatedAt(meeting(X0,X1),X2) :-
      far(X1,X0,24,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X0),X2),
      happensAt(inactive(X1),X2),
      close(X1,X0,24,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X1),X2),
      happensAt(active(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X0),X2),
      happensAt(walking(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      close(X1,X0,24,X2),
      happensAt(inactive(X0),X2).
*%

%*
initiatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,24,X2),
      happensAt(inactive(X0),X2),
      happensAt(walking(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      close(X1,X0,24,X2),
      happensAt(walking(X0),X2).
% Literal removed by hand. If we avoid adding that, all is ok
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      close(X1,X0,24,X2),
      happensAt(active(X1),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      happensAt(inactive(X0),X2),
      close(X1,X0,24,X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      happensAt(walking(X0),X2),
      close(X1,X0,24,X2).
% Last literal Removed by hand
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      happensAt(walking(X0),X2),
      close(X0,X1,25,X2).
% Added by hand, if we learn that, all is ok
%terminatedAt(meeting(X0,X1),X2) :-
%      far(X1,X0,24,X2).
*%


%-------------------------------------
%fold-6
%-------
%*
initiatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,24,X2),
      happensAt(inactive(X0),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      close(X1,X0,24,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      close(X0,X1,25,X2),
      happensAt(active(X1),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      happensAt(walking(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      happensAt(inactive(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      far(X1,X0,24,X2).
*%
%-------------------------------------




%-------------------------------------
%fold-7
%-------
%*
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(walking(X1),X2),
      happensAt(inactive(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      happensAt(walking(X0),X2).
initiatedAt(meeting(X0,X1),X2) :-
      close(X0,X1,25,X2),
      happensAt(inactive(X0),X2).
initiatedAt(meeting(X0,X1),X2) :-
      close(X0,X1,25,X2),
      happensAt(inactive(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,24,X2),
      not happensAt(disappear(X0),X2),
      close(X0,X1,25,X2).
*%


%-------------------------------------
%fold-8
%-------
%*
terminatedAt(meeting(X0,X1),X2) :-
      close(X1,X0,24,X2),
      happensAt(walking(X1),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(inactive(X1),X2),
      happensAt(walking(X0),X2).
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      happensAt(active(X1),X2),
      close(X0,X1,25,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      far(X0,X1,24,X2),
      happensAt(active(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(appear(X0),X2),
      happensAt(active(X0),X2),
      far(X1,X0,34,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      happensAt(active(X1),X2),
      close(X0,X1,25,X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      happensAt(inactive(X1),X2).
initiatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X1),X2),
      happensAt(inactive(X0),X2).
*%

%-------------------------------------
%fold-9
%-------
%*
terminatedAt(meeting(X0,X1),X2) :-
      happensAt(active(X0),X2),
      close(X1,X0,24,X2).
*%
