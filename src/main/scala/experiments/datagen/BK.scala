package experiments.datagen

/**
  * Created by nkatz on 11/8/15.
  */
object BK {

  val modes = List(
    "modeh(initiatedAt(fighting(+person,+person),+time))",
    "%modeh(initiatedAt(moving(+person,+person),+time))",
    "%modeh(initiatedAt(meeting(+person,+person),+time))",
    "modeh(terminatedAt(fighting(+person,+person),+time))",
    "%modeh(terminatedAt(moving(+person,+person),+time))",
    "%modeh(terminatedAt(meeting(+person,+person),+time))",
    "%modeh(initiatedAt(leavingObject(+person,+object),+time))",
    "%modeh(terminatedAt(leavingObject(+person,+object),+time))",
    "modeb(distLessThan(+person,+person,#threshold_value,+time))",
    "modeb(distMoreThan(+person,+person,#threshold_value,+time))",
    "modeb(distLessThan(+person,+object,#threshold_value,+time))",
    "modeb(distLessThan(+object,+person,#threshold_value,+time))",
    "modeb(orientClose(+person,+person,#orientation_threshold,+time))",
    "modeb(orientFar(+person,+person,#orientation_threshold,+time))",
    "modeb(orientClose(+person,+object,#orientation_threshold,+time))",
    "modeb(orientFar(+object,+person,#orientation_threshold,+time))",
    "modeb(holdsAt(visible(+person),+time))",
    "modeb(holdsAt(visible(+object),+time))",
    "modeb(holdsAt(invisible(+person),+time))",
    "modeb(holdsAt(invisible(+object),+time))",
    "modeb(happensAt(inactive(+object),+time))",
    "modeb(happensAt(appears(+object),+time))",
    "modeb(happensAt(disappears(+object),+time))"
  )
  val exPatterns = List(
    "examplePattern(holdsAt(fighting(+person,+person),+time))",
    "%examplePattern(holdsAt(moving(+person,+person),+time))",
    "%examplePattern(holdsAt(meeting(+person,+person),+time))",
    "%examplePattern(holdsAt(leavingObject(+person,+object),+time))"
  )

  val bk =
  """
    |
    |holdsAt(F,Te) :-
    |       fluent(F),
    |       not sdFluent(F),
    |       initiatedAt(F,Ts),
    |       Te = Ts + 1,
    |       time(Ts),time(Te).
    |
    |holdsAt(F,Te) :-
    |       fluent(F),
    |       not sdFluent(F),
    |       holdsAt(F,Ts),
    |       not terminatedAt(F,Ts),
    |       Te = Ts + 1,
    |       time(Ts),time(Te).
    |
    |holdsAt(F,T) :-
    |       initialTime(T),
    |       example(holdsAt(F,T)).
    |
    |
    |initialTime(X) :- time(X), #false : X > Y, time(Y).
    |
    |sdFluent(visible(X)):-person(X).
    |
    |entity(X) :- person(X).
    |entity(X) :- object(X).
    |
    |fluent(moving(X,Y)) :- person(X),person(Y),X != Y.
    |fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.
    |fluent(fighting(X,Y)) :- person(X),person(Y),X != Y.
    |fluent(leavingObject(X,Y)) :- person(X),entity(Y),X != Y.
    |
    |object(X) :- happensAt(inactive(X),_).
    |object(X) :- happensAt(appears(X),_).
    |object(X) :- happensAt(disappears(X),_).
    |
    |% Lua scripting language for arithmetic
    |#script (lua)
    |function eucldist(x1,y1,x2,y2)
    |   x = x1 - x2
    |   y = y1 - y2
    |   xDiff = math.abs(x)
    |   yDiff = math.abs(y)
    |   sideA = xDiff * xDiff
    |   sideB = yDiff * yDiff
    |   temp = sideA + sideB
    |   ypot = math.sqrt(temp)
    |   return ypot
    |end
    |
    |function absval(x,y)
    |   z = x-y
    |   res = math.abs(z)
    |   return z
    |end
    |#end.
    |
    |
    |
    |dist(Id1,Id2,T,Ypot):-
    |       coords(Id1,X1,Y1,T),
    |       coords(Id2,X2,Y2,T),
    |       Id1 != Id2,
    |       X = X1-X2,
    |       Y = Y1-Y2,
    |       Ypot = @eucldist(X1,Y1,X2,Y2).
    |
    |distLessThan(Id1,Id2,Threshold,Time):-
    |       dist(Id1,Id2,Time,Distance),
    |       Id1 != Id2,
    |       threshold_value(Threshold),
    |       Distance <= Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |distMoreThan(Id1,Id2,Threshold,Time):-
    |       dist(Id1,Id2,Time,Distance),
    |       Id1 != Id2,
    |       threshold_value(Threshold),
    |       Distance > Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |orientClose(Id1,Id2,Threshold,Time):-
    |       Id1 != Id2,
    |       orientation(Id1,X,Time),
    |       orientation(Id2,Y,Time),
    |       Diff = |X-Y|,
    |       orientation_threshold(Threshold),
    |       Diff <= Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |orientFar(Id1,Id2,Threshold,Time):-
    |       Id1 != Id2,
    |       orientation(Id1,X,Time),
    |       orientation(Id2,Y,Time),
    |       Diff = |X-Y|,
    |       orientation_threshold(Threshold),
    |       Diff > Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |
    |%threshold_value(25..45).
    |threshold_value(10).
    |threshold_value(25).
    |threshold_value(35).
    |threshold_value(40).
    |orientation_threshold(45).
    |
    |
  """.stripMargin


  val bkNoInertia =
  """
    |
    |
    |holdsAt(F,Te) :-
    |       fluent(F),
    |       not sdFluent(F),
    |       initiatedAt(F,Ts),
    |       Te = Ts + 1,
    |       time(Ts),time(Te).
    |
    |%holdsAt(F,Te) :-
    |%       fluent(F),
    |%       not sdFluent(F),
    |%       holdsAt(F,Ts),
    |%       not terminatedAt(F,Ts),
    |%       Te = Ts + 1,
    |%       time(Ts),time(Te).
    |
    |holdsAt(F,T) :-
    |       initialTime(T),
    |       example(holdsAt(F,T)).
    |
    |
    |initialTime(X) :- time(X), #false : X > Y, time(Y).
    |
    |sdFluent(visible(X)):-person(X).
    |
    |entity(X) :- person(X).
    |entity(X) :- object(X).
    |
    |fluent(moving(X,Y)) :- person(X),person(Y),X != Y.
    |fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.
    |fluent(fighting(X,Y)) :- person(X),person(Y),X != Y.
    |fluent(leavingObject(X,Y)) :- person(X),entity(Y),X != Y.
    |
    |object(X) :- happensAt(inactive(X),_).
    |object(X) :- happensAt(appears(X),_).
    |object(X) :- happensAt(disappears(X),_).
    |
    |
    |% Lua scripting language for arithmetic
    |#script (lua)
    |function eucldist(x1,y1,x2,y2)
    |   x = x1 - x2
    |   y = y1 - y2
    |   xDiff = math.abs(x)
    |   yDiff = math.abs(y)
    |   sideA = xDiff * xDiff
    |   sideB = yDiff * yDiff
    |   temp = sideA + sideB
    |   ypot = math.sqrt(temp)
    |   return ypot
    |end
    |
    |function absval(x,y)
    |   z = x-y
    |   res = math.abs(z)
    |   return z
    |end
    |#end.
    |
    |
    |
    |dist(Id1,Id2,T,Ypot):-
    |       coords(Id1,X1,Y1,T),
    |       coords(Id2,X2,Y2,T),
    |       Id1 != Id2,
    |       X = X1-X2,
    |       Y = Y1-Y2,
    |       Ypot = @eucldist(X1,Y1,X2,Y2).
    |
    |distLessThan(Id1,Id2,Threshold,Time):-
    |       dist(Id1,Id2,Time,Distance),
    |       Id1 != Id2,
    |       threshold_value(Threshold),
    |       Distance <= Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |distMoreThan(Id1,Id2,Threshold,Time):-
    |       dist(Id1,Id2,Time,Distance),
    |       Id1 != Id2,
    |       threshold_value(Threshold),
    |       Distance > Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |orientClose(Id1,Id2,Threshold,Time):-
    |       Id1 != Id2,
    |       orientation(Id1,X,Time),
    |       orientation(Id2,Y,Time),
    |       Diff = |X-Y|,
    |       orientation_threshold(Threshold),
    |       Diff <= Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |orientFar(Id1,Id2,Threshold,Time):-
    |       Id1 != Id2,
    |       orientation(Id1,X,Time),
    |       orientation(Id2,Y,Time),
    |       Diff = |X-Y|,
    |       orientation_threshold(Threshold),
    |       Diff > Threshold,
    |       entity(Id1),entity(Id2),time(Time).
    |
    |
    |%threshold_value(25..45).
    |threshold_value(10).
    |threshold_value(25).
    |threshold_value(35).
    |threshold_value(40).
    |orientation_threshold(45).
    |
  """.stripMargin


  val bkNoInertiaMarked =
    """
      |
      |
      |marked(I,J,holdsAt(F,Te)) :-
      |       fluent(F),
      |       rule(I),
      |       supportRule(J),
      |       not sdFluent(F),
      |       marked(I,J,initiatedAt(F,Ts)),
      |       Te = Ts + 1,
      |       time(Ts),time(Te).
      |
      |%holdsAt(F,Te) :-
      |%       fluent(F),
      |%       not sdFluent(F),
      |%       holdsAt(F,Ts),
      |%       not terminatedAt(F,Ts),
      |%       Te = Ts + 1,
      |%       time(Ts),time(Te).
      |
      |holdsAt(F,T) :-
      |       initialTime(T),
      |       example(holdsAt(F,T)).
      |
      |
      |initialTime(X) :- time(X), #false : X > Y, time(Y).
      |
      |sdFluent(visible(X)):-person(X).
      |
      |entity(X) :- person(X).
      |entity(X) :- object(X).
      |
      |fluent(moving(X,Y)) :- person(X),person(Y),X != Y.
      |fluent(meeting(X,Y)) :- person(X),person(Y),X != Y.
      |fluent(fighting(X,Y)) :- person(X),person(Y),X != Y.
      |fluent(leavingObject(X,Y)) :- person(X),entity(Y),X != Y.
      |
      |
      |object(X) :- happensAt(inactive(X),_).
      |object(X) :- happensAt(appears(X),_).
      |object(X) :- happensAt(disappears(X),_).
      |
      |% Lua scripting language for arithmetic
      |#script (lua)
      |function eucldist(x1,y1,x2,y2)
      |   x = x1 - x2
      |   y = y1 - y2
      |   xDiff = math.abs(x)
      |   yDiff = math.abs(y)
      |   sideA = xDiff * xDiff
      |   sideB = yDiff * yDiff
      |   temp = sideA + sideB
      |   ypot = math.sqrt(temp)
      |   return ypot
      |end
      |
      |function absval(x,y)
      |   z = x-y
      |   res = math.abs(z)
      |   return z
      |end
      |#end.
      |
      |
      |
      |dist(Id1,Id2,T,Ypot):-
      |       coords(Id1,X1,Y1,T),
      |       coords(Id2,X2,Y2,T),
      |       Id1 != Id2,
      |       X = X1-X2,
      |       Y = Y1-Y2,
      |       Ypot = @eucldist(X1,Y1,X2,Y2).
      |
      |distLessThan(Id1,Id2,Threshold,Time):-
      |       dist(Id1,Id2,Time,Distance),
      |       Id1 != Id2,
      |       threshold_value(Threshold),
      |       Distance <= Threshold,
      |       entity(Id1),entity(Id2),time(Time).
      |
      |distMoreThan(Id1,Id2,Threshold,Time):-
      |       dist(Id1,Id2,Time,Distance),
      |       Id1 != Id2,
      |       threshold_value(Threshold),
      |       Distance > Threshold,
      |       entity(Id1),entity(Id2),time(Time).
      |
      |orientClose(Id1,Id2,Threshold,Time):-
      |       Id1 != Id2,
      |       orientation(Id1,X,Time),
      |       orientation(Id2,Y,Time),
      |       Diff = |X-Y|,
      |       orientation_threshold(Threshold),
      |       Diff <= Threshold,
      |       entity(Id1),entity(Id2),time(Time).
      |
      |orientFar(Id1,Id2,Threshold,Time):-
      |       Id1 != Id2,
      |       orientation(Id1,X,Time),
      |       orientation(Id2,Y,Time),
      |       Diff = |X-Y|,
      |       orientation_threshold(Threshold),
      |       Diff > Threshold,
      |       entity(Id1),entity(Id2),time(Time).
      |
      |
      |%threshold_value(25..45).
      |threshold_value(10).
      |threshold_value(25).
      |threshold_value(35).
      |threshold_value(40).
      |orientation_threshold(45).
      |
    """.stripMargin



}
