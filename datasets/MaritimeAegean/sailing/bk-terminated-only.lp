#script (python)

from gringo import Fun

def less_than(x,y):
    return float(x) < float(y)

def greater_than(x,y):
    return float(x) > float(y)

#end.

%---------------------------------------------------------------------------
% The initiation axiom is needed here to ensure that we won't get an UNSAT
% error in case a fluent is initiated in the middle of a training sequence.
% So we abduce initiatedAt atoms as well, but we filter them out before
% kernel set construction (with the learningTerminatedOnly parameter).
%---------------------------------------------------------------------------

holdsAt(F, Te) :-
  fluent(F), time(Te),
  initiatedAt(F, Ts),
  right_before(Ts, Te).

holdsAt(F, Te) :-
  fluent(F),
  holdsAt(F,Ts),
  not terminatedAt(F,Ts),
  right_before(Ts, Te),
  time(Te), time(Ts).

%------------------------------------------------------------------------------------
% We'll add this however, to make sure that splitting the data won't cause problems
% with HLEs that hold from previous windows and occur at the first time point.
%-------------------------------------------------------------------------------------

holdsAt(F,T) :-
  initialTime(T),
  example(holdsAt(F,T)).

initialTime(X) :- time(X), #false : @greater_than(X,Y) = 1, time(Y).
right_before(X,Z) :- time(X), #false : @less_than(X,Y) = 1, time(Y), @less_than(Y,Z) = 1 ; time(Z), @less_than(X,Z) = 1.

%--------------------------------------------------------------------------------------------
% the overall initial time, extracted by the rule
% initialTime(X) :- time(X), #false : X > Y, time(Y).
% may differ from the time of the first observed example.
% Since we do not have an axiom to initiate the corresponding fluent
% here (we are learning terminatedAt only), we need to add an additional rule
% that extract the examples' initial time:
% examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).
% and an additional axiom as below:
%--------------------------------------------------------------------------------------------

holdsAt(F,T) :-
  fluent(F),
  examplesInitialTime(T),
  example(holdsAt(F,T)).

examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).


fluent(highSpeedIn(V,A)) :- vessel(V), area(A).
fluent(withinArea(V,A)) :- vessel(V), area(A).
fluent(loitering(V)) :- vessel(V).
fluent(lowSpeed(V)) :- vessel(V).
fluent(rendezVouz(V1,V2)) :- vessel(V1), vessel(V2).
fluent(sailing(V)) :- vessel(V).
fluent(stopped(V)) :- vessel(V).

vessel(V) :- example(holdsAt(highSpeedIn(V,_),_)).
vessel(V) :- example(holdsAt(withinArea(V,_),_)).
vessel(V) :- example(holdsAt(loitering(V),_)).
vessel(V) :- example(holdsAt(lowSpeed(V),_)).
vessel(V) :- example(holdsAt(rendezVouz(V,_),_)).
vessel(V) :- example(holdsAt(rendezVouz(_,V),_)).
vessel(V) :- example(holdsAt(sailing(V),_)).
vessel(V) :- example(holdsAt(stopped(V),_)).

vessel(V) :- happensAt(gap_start(V),_).
vessel(V) :- happensAt(gap_end(V),_).
vessel(V) :- happensAt(change_in_speed_start(V),_).
vessel(V) :- happensAt(change_in_speed_end(V),_).
vessel(V) :- happensAt(slow_motion_start(V),_).
vessel(V) :- happensAt(slow_motion_end(V),_).
vessel(V) :- happensAt(stop_start(V),_).
vessel(V) :- happensAt(stop_end(V),_).
vessel(V) :- happensAt(velocity(V,_),_).
vessel(V) :- happensAt(change_in_heading(V),_).
vessel(V) :- happensAt(isInArea(V,_),_).
vessel(V) :- happensAt(leavesArea(V,_),_).
vessel(V) :- notCloseToPorts(V,_).

time(T) :- happensAt(gap_start(_),T).
time(T) :- happensAt(gap_end(_),T).
time(T) :- happensAt(stop_start(_),T).
time(T) :- happensAt(stop_end(_),T).
time(T) :- happensAt(change_in_speed_start(_),T).
time(T) :- happensAt(change_in_speed_end(_),T).
time(T) :- happensAt(slow_motion_start(_),T).
time(T) :- happensAt(slow_motion_end(_),T).
time(T) :- happensAt(velocity(V,_),T).
time(T) :- happensAt(change_in_heading(_),T).
time(T) :- happensAt(isInArea(_,_),T).
time(T) :- happensAt(leavesArea(_,_),T).
time(T) :- notCloseToPorts(_,T).

area(A) :- example(holdsAt(highSpeedIn(_,A),_)).
area(A) :- example(holdsAt(withinArea(_,A),_)).
area(A) :- happensAt(isInArea(_,A),_).
area(A) :- happensAt(leavesArea(_,A),_).
area(A) :- speedLimit(A,_).

speed(S) :- speedLimit(_,S).
speed(S) :- happensAt(velocity(_,S),_).
speed(S) :- velocity(S).

%velocity(1..10).
%velocity(1..4).
velocity(2).

before(X,Z) :- time(X), time(Z), X < Z.
before(X,Z) :- time(Y), before(X,Y), before(Y,Z).

right_before(X,Z) :- time(X), #false : X < Y, time(Y), Y < Z ; time(Z), X < Z.

happensAt(tooFastForArea(V,A),T) :-
  vessel(V), area(A),
  happensAt(velocity(V,S),T),
  speedLimit(A,L),
  @greater_than(S, L) = 1.

happensAt(speedOk(V,A),T) :-
  vessel(V), area(A),
  happensAt(velocity(V,S),T),
  speedLimit(A,L),
  @less_than(S, L) = 1.  

grThan(X,Y) :- @greater_than(X,Y) = 1, speed(X), speed(Y).
lessThan(X,Y) :- @less_than(X,Y) = 1,speed(X), speed(Y).