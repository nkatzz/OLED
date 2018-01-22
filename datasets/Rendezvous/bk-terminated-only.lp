
% The initiation axiom is needed here to ensure that we won't get an UNSAT
% error in case a fluent is initiated in the middle of a training sequence.
% So we abduce initiatedAt atoms as well, but we filter them out before
% kernel set construction (with the learningTerminatedOnly parameter).

holdsAt(F,Te) :-
       fluent(F),
       initiatedAt(F,Ts),
       Te = Ts + 1,
       time(Ts),time(Te).

holdsAt(F,Te) :-
      fluent(F),
      holdsAt(F,Ts),
      not terminatedAt(F,Ts),
      Te = Ts + 1,
      time(Ts),time(Te).

holdsAt(F,T) :-
       initialTime(T),
       example(holdsAt(F,T)).

initialTime(X) :- time(X), #false : X > Y, time(Y).


% the overall initial time, extracted by the rule
% initialTime(X) :- time(X), #false : X > Y, time(Y).
% may differ from the time of the first observed example.
% Since we do not have an axiom to initiate the corresponding fluent
% here (we are learning terminatedAt only), we need to add an additional rule
% that extract the examples' initial time:
% examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).
% and an additional axiom as below:

holdsAt(F,T) :-
       fluent(F),
       examplesInitialTime(T),
       example(holdsAt(F,T)).

examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).

fluent(rendezvous(X,Y)) :- vessel(X),vessel(Y),X != Y.

vessel(X) :- happensAt(change_in_heading(X),_).
vessel(X) :- happensAt(change_in_speed_end(X),_).
vessel(X) :- happensAt(change_in_speed_start(X),_).
vessel(X) :- happensAt(gap_end(X),_).
vessel(X) :- happensAt(gap_start(X),_).
vessel(X) :- happensAt(proximity(X,_),_).
vessel(X) :- happensAt(proximity(_,X),_).
vessel(X) :- happensAt(slow_motion_start(X),_).
vessel(X) :- happensAt(slow_motion_end(X),_).
vessel(X) :- happensAt(stop_start(X),_).
vessel(X) :- happensAt(stop_end(X),_).

time(X) :- happensAt(change_in_heading(_),X).
time(X) :- happensAt(change_in_speed_end(_),X).
time(X) :- happensAt(change_in_speed_start(_),X).
time(X) :- happensAt(gap_end(_),X).
time(X) :- happensAt(gap_start(_),X).
time(X) :- happensAt(proximity(_,_),X).
time(X) :- happensAt(slow_motion_start(_),X).
time(X) :- happensAt(slow_motion_end(_),X).
time(X) :- happensAt(stop_start(_),X).
time(X) :- happensAt(stop_end(_),X).

vessel(X) :- example(holdsAt(rendezvous(X,_),_)).
vessel(X) :- example(holdsAt(rendezvous(_,X),_)).
time(X) :- example(holdsAt(rendezvous(_,_),X)).
