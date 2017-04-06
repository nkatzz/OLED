%% time(1..30001).
%person(daniel;sandra;mary;john).
%place(hallway;garden;office;kitchen;bathroom;bedroom).

%holdsAt(F,T+1):- fluent(F),time(T), initiatedAt(F,T).
%holdsAt(F,T+1):- fluent(F),time(T), holdsAt(F,T), not terminatedAt(F,T).




holdsAt(F,T):-
   initiatedAt(F,Ts),
   Ts < T, fluent(F),
   time(T), time(Ts),
   not clipped(Ts,F,T).

clipped(Ts,F,T) :-
   time(T), time(Ts), time(Tm), fluent(F),
   Ts < Tm, Tm < T,
   terminatedAt(F,Tm).


%% I don't think this is ever needed
%holdsAt(F,T) :-
%   initialTime(T),
%   example(holdsAt(F,T)).




fluent(location(X,Y)):-person(X),place(Y).
event(go(X,Y)):-person(X),place(Y).
event(go_back(X,Y)):-person(X),place(Y).
event(move(X,Y)):-person(X),place(Y).
event(travel(X,Y)):-person(X),place(Y).
event(journey(X,Y)):-person(X),place(Y).

initialTime(X) :- time(X), #false : X > Y, time(Y).
lastTime(X) :- time(X), #false : X < Y, time(Y).
annotation_time(T) :- example(holdsAt(F,T)), fluent(F).
annotation_time(T) :- negExample(holdsAt(F,T)), fluent(F).
time(T):- happensAt(E,T),event(E).
time(T):- annotation_time(T).
time(T):- time(T1), T = T1+1, lastTime(T2), T <= T2 . % fill in the gaps

person(X) :- example(holdsAt(location(X,_),_)).
person(X) :- negExample(holdsAt(location(X,_),_)).
person(X) :- happensAt(go(X,_),_).
person(X) :- happensAt(go_back(X,_),_).
person(X) :- happensAt(move(X,_),_).
person(X) :- happensAt(travel(X,_),_).
person(X) :- happensAt(journey(X,_),_).

place(X) :- example(holdsAt(location(_,X),_)).
place(X) :- negExample(holdsAt(location(_,X),_)).
place(X) :- happensAt(go(_,X),_).
place(X) :- happensAt(go_back(_,X),_).
place(X) :- happensAt(move(_,X),_).
place(X) :- happensAt(travel(_,X),_).
place(X) :- happensAt(journey(_,X),_).

:- initiatedAt(location(X,Y),T), not happensInvolving(X,Y,T). % add also the place
:- terminatedAt(location(X,Y),T), not happensInvolving(X,T).  % add only the person

happensInvolving(X,T) :- happensAt(go(X,Y),T),place(Y).
happensInvolving(X,T) :- happensAt(go_back(X,Y),T),place(Y).
happensInvolving(X,T) :- happensAt(move(X,Y),T),place(Y).
happensInvolving(X,T) :- happensAt(travel(X,Y),T),place(Y).
happensInvolving(X,T) :- happensAt(journey(X,Y),T),place(Y).

happensInvolving(X,Y,T) :- happensAt(go(X,Y),T).
happensInvolving(X,Y,T) :- happensAt(go_back(X,Y),T).
happensInvolving(X,Y,T) :- happensAt(move(X,Y),T).
happensInvolving(X,Y,T) :- happensAt(travel(X,Y),T).
happensInvolving(X,Y,T) :- happensAt(journey(X,Y),T).



:- initiatedAt(F,T), terminatedAt(F,T), fluent(F), time(T). % A fluent cannot be initiated and terminated at the same time.
:- holdsAt(location(X,Y1),T), holdsAt(location(X,Y2),T), Y1 != Y2.    % A person cannot be in two places at the same time.
% constrain initiations/terminations on the same person:
%:- initiatedAt(go(X,_),location(Y,_),_),X!=Y.
%:- initiatedAt(go_back(X,_),location(Y,_),_),X!=Y.
%:- initiatedAt(move(X,_),location(Y,_),_),X!=Y.
%:- initiatedAt(travel(X,_),location(Y,_),_),X!=Y.
%:- initiatedAt(journey(X,_),location(Y,_),_),X!=Y.
%
%:- terminatedAt(go(X,_),location(Y,_),_),X!=Y.
%:- terminatedAt(go_back(X,_),location(Y,_),_),X!=Y.
%:- terminatedAt(move(X,_),location(Y,_),_),X!=Y.
%:- terminatedAt(travel(X,_),location(Y,_),_),X!=Y.
%:- terminatedAt(journey(X,_),location(Y,_),_),X!=Y.


neq(X,Y):-place(X),place(Y),X!=Y.
lessThan(T,S):-time(T),time(S), T<S.
next(T,S):-time(T),time(S), T=S+1.

