tps(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: example( holdsAt(rendezvous(X0,X1),Te) ), marked(I, initiatedAt(rendezvous(X0,X1),Ts) ), Te = Ts + 1, time(Te), time(Ts) }.
fps(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: not example( holdsAt(rendezvous(X0,X1),Te) ), marked(I, initiatedAt(rendezvous(X0,X1),Ts) ), Te = Ts + 1, time(Te), time(Ts) }.
fns(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: example( holdsAt(rendezvous(X0,X1),Te) ), not marked(I, initiatedAt(rendezvous(X0,X1),Ts) ), Te = Ts + 1, time(Te), time(Ts) }.

initialTime(X) :- time(X), #false : X > Y, time(Y).

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