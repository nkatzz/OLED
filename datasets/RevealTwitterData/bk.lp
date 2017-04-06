holdsAt(F,Te) :- fluent(F), initiatedAt(F,Ts), Te = Ts + 1, time(Ts),time(Te).
holdsAt(F,Te) :- fluent(F), holdsAt(F,Ts), not terminatedAt(F,Ts), Te = Ts + 1, time(Ts),time(Te).
holdsAt(F,T) :- initialTime(T), example(holdsAt(F,T)).

initialTime(X) :- time(X), #false : X > Y, time(Y).

fluent(continuing(X)) :- community(X).

community(X):- happensAt(f1(X,_),_).
community(X):- happensAt(f2(X,_),_).
community(X):- happensAt(f3(X,_),_).
community(X):- happensAt(f4(X,_),_).

featureValue(X) :- happensAt(f1(_,X),_).
featureValue(X) :- happensAt(f2(_,X),_).
featureValue(X) :- happensAt(f3(_,X),_).
featureValue(X) :- happensAt(f4(_,X),_).

time(X) :- happensAt(f1(_,_),X).
time(X) :- happensAt(f2(_,_),X).
time(X) :- happensAt(f3(_,_),X).
time(X) :- happensAt(f4(_,_),X).
time(X) :- example(holdsAt(continuing(_),X)).


