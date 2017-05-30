tps(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: example( holdsAt(moving(X0,X1),Te) ), not marked(I, terminatedAt(moving(X0,X1),Ts) ), Te = Ts + 40 }.

tps(I, X) :-
  rule(I),
  X = #count {X0,X1,Te,Ts: example( holdsAt(moving(X0,X1),Ts) ),
                           not example( holdsAt(moving(X0,X1),Te) ),
                           marked(I, terminatedAt(moving(X0,X1),Ts) ), Te = Ts + 40 }.

fns(I, X) :- rule(I), X = #count {X0,X1,Te,Ts: example( holdsAt(moving(X0,X1),Te) ), marked(I, terminatedAt(moving(X0,X1),Ts) ), Te = Ts + 40 }.

examplesInitialTime(X) :- example(holdsAt(_,X)), #false : X > Y, example(holdsAt(_,Y)).

initialTime(X) :- time(X), #false : X > Y, time(Y).

sdFluent(visible(X)):-person(X).

fluent(moving(X,Y)) :- person(X),person(Y),X != Y.
fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.
fluent(fighting(X,Y)) :- person(X),person(Y),X != Y.
fluent(leavingObject(X,Y)) :- person(X),person(Y),X != Y.

person(X):-
    happensAt(walking(X),_).
person(X):-
    happensAt(running(X),_).
person(X):-
    happensAt(active(X),_).
person(X):-
    happensAt(inactive(X),_).
person(X):-
    happensAt(abrupt(X),_).
person(X):-
    happensAt(appear(X),_).
person(X):-
    happensAt(disappear(X),_).
person(X):-
    orientation(X,_,_).
person(X):-
    coords(X,_,_,_).


time(T):-
    example(holdsAt(_,T)).
time(X):-
    happensAt(walking(_),X).
time(X):-
    happensAt(active(_),X).
time(X):-
    happensAt(inactive(_),X).
time(X):-
    happensAt(abrupt(_),X).
time(X):-
    happensAt(running(_),X).
time(X):-
    happensAt(appear(_),X).
time(X):-
    happensAt(disappear(_),X).
time(X):-
    orientation(_,_,X).
time(X):-
    coords(_,_,_,X).

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

close(Id1,Id2,Threshold,Time):-
       dist(Id1,Id2,Time,Distance),
       Id1 != Id2,
       threshold_value(Threshold),
       Distance <= Threshold,
       person(Id1),person(Id2),time(Time).

far(Id1,Id2,Threshold,Time):-
       dist(Id1,Id2,Time,Distance),
       Id1 != Id2,
       threshold_value(Threshold),
       Distance > Threshold,
       person(Id1),person(Id2),time(Time).

orientationMove(Id1,Id2,Time):-
       Id1 != Id2,
       orientation(Id1,X,Time),
       orientation(Id2,Y,Time),
       Diff = |X-Y|,
       orientation_threshold(Threshold),
       Diff <= Threshold,
       person(Id1),person(Id2),time(Time).

orientationFar(Id1,Id2,Time):-
       Id1 != Id2,
       orientation(Id1,X,Time),
       orientation(Id2,Y,Time),
       Diff = |X-Y|,
       orientation_threshold(Threshold),
       Diff > Threshold,
       person(Id1),person(Id2),time(Time).

orientation_threshold(45).
%orientation_threshold(100).
%orientation_threshold(200).
%orientation_threshold(300).
%orientation_threshold(360).

threshold_value(25).
threshold_value(24).
threshold_value(27).
threshold_value(34).
threshold_value(40).

%threshold_value(5..10). %move
%threshold_value(30). %meet
%threshold_value(20). %fight

% This is to avoid a 'warning: atom is undefined:' warning
% see http://sourceforge.net/p/potassco/mailman/message/31768234/ for details
%#program warnings.
%example(0).
%use3(0,0,0).

