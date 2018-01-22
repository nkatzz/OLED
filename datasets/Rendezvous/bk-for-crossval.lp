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

%initialTime(X) :- time(X), #false : X > Y, time(Y).

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