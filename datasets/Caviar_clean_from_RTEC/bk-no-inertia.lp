holdsAt(F,Te) :-
       fluent(F),
       not sdFluent(F),
       initiatedAt(F,Ts),
       Te = Ts + 40,
       time(Ts),time(Te).

holdsAt(F,T) :-
       not sdFluent(F),
       initialTime(T),
       example(holdsAt(F,T)).


sdFluent(visible(X)):- entity(X).
sdFluent(person(X)):- entity(X).
sdFluent(close(X,Y,D)) :- entity(X),entity(Y),threshold_value(D).
sdFluent(far(X,Y,D)) :- entity(X),entity(Y),threshold_value(D).
sdFluent(sameDirection(X,Y)) :- entity(X),entity(Y).
sdFluent(diffDirection(X,Y)) :- entity(X),entity(Y).

initialTime(X) :- time(X), #false : X > Y, time(Y).

holdsAt(person(X),T) :-
   happensAt(walking(X),T).
holdsAt(person(X),T) :-
   happensAt(active(X),T).
holdsAt(person(X),T) :-
   happensAt(abrupt(X),T).
holdsAt(person(X),T) :-
   happensAt(running(X),T).






fluent(moving(X,Y)) :- entity(X),entity(Y),X != Y.
fluent(moving(X,Y)) :- example(holdsAt(moving(X,Y),_)).
fluent(meeting(X,Y)) :- entity(X),entity(Y),X != Y.
fluent(meeting(X,Y)) :- example(holdsAt(meeting(X,Y),_)).
fluent(fighting(X,Y)) :- entity(X),entity(Y),X != Y.
fluent(fighting(X,Y)) :- example(holdsAt(fighting(X,Y),_)).
fluent(leavingObject(X,Y)) :- entity(X),entity(Y),X != Y.
fluent(leavingObject(X,Y)) :- example(holdsAt(leavingObject(X,Y),_)).

entity(X):- happensAt(abrupt(X),_).
entity(X):- happensAt(running(X),_).
entity(X):- happensAt(active(X),_).
entity(X):- happensAt(walking(X),_).
entity(X):- happensAt(inactive(X),_).
entity(X):- happensAt(appear(X),_).
entity(X):- happensAt(disappear(X),_).
entity(X):- orientation(X,_,_).
entity(X):- coords(X,_,_,_).
entity(X):- holdsAt(visible(X),_).


time(T):- orientation(_,_,T).
time(T):- coords(_,_,_,T).
time(X):- happensAt(abrupt(_),X).
time(X):- happensAt(running(_),X).
time(X):- happensAt(active(_),X).
time(X):- happensAt(walking(_),X).
time(X):- happensAt(inactive(_),X).
time(T):- example(holdsAt(_,T)).
time(T):- happensAt(appear(_),T).
time(T):- happensAt(disappear(_),T).

not holdsAt(close(X,Y,D),T) :- not holdsAt(close(X,Y,D),T), entity(X), entity(Y), X != Y, threshold_value(D), time(T).
not holdsAt(person(X),T) :- not holdsAt(person(X),T), entity(X), time(T).


%*
initiatedAt(meeting(X,Y),T) :-
  happensAt(active(X),T),
  happensAt(active(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

initiatedAt(meeting(X,Y),T) :-
  happensAt(inactive(X),T),
  happensAt(inactive(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

initiatedAt(meeting(X,Y),T) :-
  happensAt(active(X),T),
  happensAt(inactive(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

initiatedAt(meeting(X,Y),T) :-
  happensAt(inactive(X),T),
  happensAt(active(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

initiatedAt(meeting(X,Y),T) :-
  happensAt(walking(X),T),
  happensAt(active(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

initiatedAt(meeting(X,Y),T) :-
  happensAt(active(X),T),
  happensAt(walking(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

initiatedAt(meeting(X,Y),T) :-
  happensAt(walking(X),T),
  happensAt(inactive(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

initiatedAt(meeting(X,Y),T) :-
  happensAt(inactive(X),T),
  happensAt(walking(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,25),T).

terminatedAt(meeting(X,Y),T) :-
  happensAt(running(X),T),
  entity(Y),time(T).

terminatedAt(meeting(X,Y),T) :-
  happensAt(running(Y),T),
  entity(X),time(T).

terminatedAt(meeting(X,Y),T) :-
  happensAt(abrupt(X),T),
  entity(Y),time(T).

terminatedAt(meeting(X,Y),T) :-
  happensAt(abrupt(Y),T),
  entity(X),time(T).

terminatedAt(meeting(X,Y),T) :-
  happensAt(disappear(Y),T),
  entity(X),time(T).

terminatedAt(meeting(X,Y),T) :-
  happensAt(disappear(X),T),
  entity(Y),time(T).

terminatedAt(meeting(X,Y),T) :-
  holdsAt(far(X,Y,34),T).





initiatedAt(moving(X,Y),T) :-
  happensAt(walking(X),T),
  happensAt(walking(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,34),T),
  holdsAt(sameDirection(X,Y),T).

terminatedAt(moving(X,Y),T) :-
  not happensAt(walking(X),T),
  entity(X),entity(Y),time(T).

terminatedAt(moving(X,Y),T) :-
  not happensAt(walking(Y),T),
  entity(X),entity(Y),time(T).

terminatedAt(moving(X,Y),T) :-
  holdsAt(far(X,Y,34),T).

terminatedAt(moving(X,Y),T) :-
  holdsAt(diffDirection(X,Y),T).

terminatedAt(moving(X,Y),T) :-
  happensAt(disappear(Y),T),
  entity(X),time(T).

terminatedAt(moving(X,Y),T) :-
  happensAt(disappear(X),T),
  entity(Y),time(T).




initiatedAt(fighting(X,Y),T) :-
  happensAt(abrupt(X),T),
  happensAt(abrupt(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  holdsAt(close(X,Y,24),T).

initiatedAt(fighting(X,Y),T) :-
  happensAt(abrupt(X),T),
  not happensAt(inactive(Y),T),
  not happensAt(running(Y),T),
  not happensAt(walking(Y),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  entity(Y),
  holdsAt(close(X,Y,24),T).

initiatedAt(fighting(X,Y),T) :-
  happensAt(abrupt(Y),T),
  not happensAt(inactive(X),T),
  not happensAt(running(X),T),
  not happensAt(walking(X),T),
  not happensAt(disappear(X),T),
  not happensAt(disappear(Y),T),
  entity(X),
  holdsAt(close(X,Y,24),T).

terminatedAt(fighting(X,Y),T) :-
  not happensAt(abrupt(X),T),
  not happensAt(abrupt(Y),T),
  entity(X),entity(Y),time(T).

terminatedAt(fighting(X,Y),T) :-
  holdsAt(far(X,Y,24),T).

terminatedAt(fighting(X,Y),T) :-
  holdsAt(diffDirection(X,Y),T).

terminatedAt(fighting(X,Y),T) :-
  happensAt(disappear(Y),T),
  entity(X),time(T).

terminatedAt(fighting(X,Y),T) :-
  happensAt(disappear(X),T),
  entity(Y),time(T).


initiatedAt(leavingObject(Person,Object),T) :-
  happensAt(appear(Object),T),
  happensAt(inactive(Object),T),
  holdsAt(person(Person),T),
  holdsAt(close(Person,Object,30),T).

terminatedAt(leavingObject(Person,Object),T) :-
  happensAt(disappear(Object),T),
  entity(Person).



debug(close(X,Y,D),T) :- holdsAt(close(X,Y,D),T).
debug(far(X,Y,D),T) :- holdsAt(far(X,Y,D),T).
debug(sameDirection(X,Y),T) :- holdsAt(sameDirection(X,Y),T).
debug(diffDirection(X,Y),T) :- holdsAt(diffDirection(X,Y),T).
*%

% Lua scripting language for arithmetic
#script (lua)
function eucldist(x1,y1,x2,y2)
   x = x1 - x2
   y = y1 - y2
   xDiff = math.abs(x)
   yDiff = math.abs(y)
   sideA = xDiff * xDiff
   sideB = yDiff * yDiff
   temp = sideA + sideB
   ypot = math.sqrt(temp)
   return ypot
end

function absval(x,y)
   z = x-y
   res = math.abs(z)
   return z
end
#end.



dist(Id1,Id2,T,Ypot):-
       coords(Id1,X1,Y1,T),
       coords(Id2,X2,Y2,T),
       Id1 != Id2,
       X = X1-X2,
       Y = Y1-Y2,
       Ypot = @eucldist(X1,Y1,X2,Y2).

holdsAt(close(Id1,Id2,Threshold),Time):-
       dist(Id1,Id2,Time,Distance),
       Id1 != Id2,
       threshold_value(Threshold),
       Distance <= Threshold,
       entity(Id1),entity(Id2),time(Time).

holdsAt(far(Id1,Id2,Threshold),Time):-
       dist(Id1,Id2,Time,Distance),
       Id1 != Id2,
       threshold_value(Threshold),
       Distance > Threshold,
       entity(Id1),entity(Id2),time(Time).

holdsAt(sameDirection(Id1,Id2),Time):-
       Id1 != Id2,
       orientation(Id1,X,Time),
       orientation(Id2,Y,Time),
       Diff = |X-Y|,
       orientation_threshold(Threshold),
       Diff <= Threshold,
       entity(Id1),entity(Id2),time(Time).

holdsAt(diffDirection(Id1,Id2),Time):-
       Id1 != Id2,
       orientation(Id1,X,Time),
       orientation(Id2,Y,Time),
       Diff = |X-Y|,
       orientation_threshold(Threshold),
       Diff > Threshold,
       entity(Id1),entity(Id2),time(Time).

threshold_value(24).
threshold_value(25).
threshold_value(30).
threshold_value(34).
orientation_threshold(45).

%*
#show.
#show holdsAt(moving(X,Y),T):holdsAt(moving(X,Y),T).
#show holdsAt(meeting(X,Y),T):holdsAt(meeting(X,Y),T).
#show holdsAt(fighting(X,Y),T):holdsAt(fighting(X,Y),T).
#show holdsAt(leavingObject(X,Y),T):holdsAt(leavingObject(X,Y),T).
#show debug(close(X,Y,D),T):holdsAt(close(X,Y,D),T).
#show debug(far(X,Y,D),T):holdsAt(far(X,Y,D),T).
#show debug(sameDirection(X,Y),T):holdsAt(sameDirection(X,Y),T).
#show debug(diffDirection(X,Y),T):holdsAt(diffDirection(X,Y),T).
*%