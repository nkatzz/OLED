#script (python)

from gringo import Fun

def less_than(x,y):
    return float(x) < float(y)

def greater_than(x,y):
    return float(x) > float(y)

#end.

%% Necessary background theory
initiatedAt(stopped(Vessel), T) :-
    happensAt(stop_start(Vessel),T),
    notCloseToPorts(Vessel,T),
    not happensAt(stop_end(Vessel), T).

terminatedAt(stopped(Vessel), T) :-
    happensAt(stop_end(Vessel), T).



tps(I, X) :- rule(I), X = #count {V,Te,Ts: example( holdsAt(lowSpeed(V),Te) ), marked(I, initiatedAt(lowSpeed(V),Ts) ), right_before(Ts, Te) }.
fps(I, X) :- rule(I), X = #count {V,Te,Ts: not example( holdsAt(lowSpeed(V),Te) ), marked(I, initiatedAt(lowSpeed(V),Ts) ), time(Te), right_before(Ts, Te) }.

initialTime(X) :- time(X), #false : @greater_than(X,Y) = 1, time(Y).
right_before(X,Z) :- time(X), #false : @less_than(X,Y) = 1, time(Y), @less_than(Y,Z) = 1 ; time(Z), @less_than(X,Z) = 1.

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

time(T) :- example(holdsAt(_,T)).
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

velocity(1..10).

grThan(X,Y) :- @greater_than(X,Y) = 1, speed(X), speed(Y).
lessThan(X,Y) :- @less_than(X,Y) = 1,speed(X), speed(Y).

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

