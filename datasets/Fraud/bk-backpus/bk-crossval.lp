%-----------------------------------------------------------------
% This is necessary when the clingo binary is used instead of jep
%-----------------------------------------------------------------


#script (python)

from gringo import Fun

"""
This is a workaround to get globals if they are deleted by shutting down jep
(this is not a problem anymore).

builtins = [x for x in (1).__class__.__base__.__subclasses__() if x.__name__ == 'catch_warnings'][0]()._module.__builtins__
__import__ = builtins['__import__']
float = builtins['float']
int = builtins['int']
gringo = __import__('gringo')
Fun = gringo.Fun
"""

def quantize_amount(amount):
    x = float(amount)
    if x < 1:
        return Fun('tiny_amount')
    elif x < 50:
        return Fun('very_small_amount')
    elif x < 100:
        return Fun('small_amount')
    elif x < 200:
        return Fun('medium_amount')
    elif x < 300:
        return Fun('high_amount')
    elif x < 400:
        return Fun('very_high_amount')
    elif x < 200000:
        return Fun('massive_amount')


def predicate_quantize_amount(amount):
    x = float(amount)
    if x < 1:
        return Fun( 'tiny_amount(%s)' % (amount) )
    elif x < 50:
        return Fun('very_small_amount(%s)' % (amount))
    elif x < 100:
        return Fun('small_amount(%s)' % (amount) )
    elif x < 200:
        return Fun('medium_amount(%s)' % (amount))
    elif x < 300:
        return Fun('high_amount(%s)' % (amount))
    elif x < 400:
        return Fun('very_high_amount(%s)' % (amount))
    elif x < 200000:
        return Fun('massive_amount(%s)' % (amount))


def less_than(x,y):
    return float(x) < float(y)

def greater_than(x,y):
    return float(x) > float(y)

def equal(x,y):
    return float(x) == float(y)

def less_than_int(x,y):
    return int(x) < int(y)

def greater_than_int(x,y):
    return int(x) > int(y)

def less_than_int_or_eq(x,y):
    return int(x) <= int(y)

def compare_win_length(start, end, span):
    x = int(start)
    y = int(end)
    z = (y - x)/60000.0
    return int(z) <= int(span)

#end.




%-------------------------
% Quantization of amounts
%-------------------------
amount(small_amount).
amount(high_amount).
amount(very_high_amount).
amount(very_small_amount).
amount(medium_amount).
amount(tiny_amount).
amount(massive_amount).

%--------------------------
% Attributes definitions
%--------------------------

% cnp(N) :- transaction(_,N,_,_,_,_,_,_,_,_).    % currently not used, but may be used with real data
% expDate(D) :- transaction(_,_,_,_,D,_,_,_,_,_).% currently not used, but may be used with real data
% country(C) :-transaction(_,_,_,_,_,C,_,_,_,_). % not used anywhere
% cvvVal(V) :-transaction(_,_,_,_,_,_,_,V,_,_).  % currently not used, but may be used with real data

cardId(Card) :- transaction(_,_,_,Card,_,_,_,_,_,_).
time(T) :- transaction(T,_,_,_,_,_,_,_,_,_).
amountNum(N) :- transaction(_,_,N,_,_,_,_,_,_,_).
acqCountry(C) :-transaction(_,_,_,_,_,_,C,_,_,_).

%fraudTime(Card, Time) :- transaction(Time,_,_,Card,_,_,_,_,fraud,_).
fraudTime(Card, Time) :- transaction(Time,_,_,Card,_,_,_,_,_,_).
%transaction(Card, @quantize_amount(Amount), Time) :- transaction(Time,_,Amount,Card,_,_,_,_,_,_).

transaction(Card, increasing, Time) :-
   transaction(Time,_,Amount,Card,_,_,_,_,_,_),
   transaction(Time1,_,Amount1,Card,_,_,_,_,_,_),
   right_before(Time1, Time),
   @less_than(Amount1, Amount) = 1.

transaction(Card, decreasing, Time) :-
   transaction(Time,_,Amount,Card,_,_,_,_,_,_),
   transaction(Time1,_,Amount1,Card,_,_,_,_,_,_),
   right_before(Time1, Time),
   @greater_than(Amount1, Amount) = 1.

transaction(Card, fraud_start, Time) :-
   transaction(Time,_,Amount,Card,_,_,_,_,_,_),
   fraudStartTime(Card, Time).


trend(increasing).
trend(decreasing).
trend(fraud_start).

small_amount(Time) :- transaction(Time,_,Amount,_,_,_,_,_,_,_), @quantize_amount(Amount) = small_amount.
high_amount(Time) :- transaction(Time,_,Amount,_,_,_,_,_,_,_), @quantize_amount(Amount) = high_amount.
very_high_amount(Time) :- transaction(Time,_,Amount,_,_,_,_,_,_,_), @quantize_amount(Amount) = very_high_amount.
very_small_amount(Time) :- transaction(Time,_,Amount,_,_,_,_,_,_,_), @quantize_amount(Amount) = very_small_amount.
medium_amount(Time) :- transaction(Time,_,Amount,_,_,_,_,_,_,_), @quantize_amount(Amount) = medium_amount.
tiny_amount(Time) :- transaction(Time,_,Amount,_,_,_,_,_,_,_), @quantize_amount(Amount) = tiny_amount.
massive_amount(Time) :- transaction(Time,_,Amount,_,_,_,_,_,_,_), @quantize_amount(Amount) = massive_amount.

winSpan(1).
winSpan(2).
winSpan(3).
winSpan(4).
winSpan(5).
winSpan(7).
winSpan(15).
winSpan(20).

% after(X,Z) :- time(X), #false : X > Y, time(Y), Y > Z ; time(Z), X > Z.
right_before(X,Z) :- time(X), #false : X < Y, time(Y), Y < Z ; time(Z), X < Z.

after(X,Z) :- time(X), time(Z), X > Z.
after(X,Z) :- time(Y), after(X,Y), after(Y,Z).

before(X,Z) :- time(X), time(Z), X < Z.
before(X,Z) :- time(Y), before(X,Y), before(Y,Z).

acqCountry(Card, AC, T) :- transaction(T,_,_,Card,_,_,AC,_,_,_).

diffAcquirer(Card, T2) :-
  acqCountry(Card, AC1, T1),
  acqCountry(Card, AC2, T2),
  before(T1, T2),
  AC1 != AC2.

diffAcquirer(Card, T1) :-
  acqCountry(Card, AC1, T1),
  acqCountry(Card, AC2, T2),
  before(T2, T1),
  AC1 != AC2.

%% this differs from fraudWithin/3 at bk-bottom-clause
fraudWithin(T1, T2, S) :- fraudInterval(_, T1, T2), winSpan(S), @compare_win_length(T1, T2, S) = 1.

fraudInterval(Card, Start, End) :- fraudStartTime(Card, Start), fraudEndTime(Card, End).

fraudStartTime(Card, X) :- fraudTime(Card, X), #false : @greater_than_int(X,Y) = 1, fraudTime(Card, Y).

fraudEndTime(Card, X) :- fraudTime(Card, X), #false : @less_than_int(X,Y) = 1, fraudTime(Card, Y).

fraudEndTime_score(Card, X) :- fraudTime_score(Card, X), #false : @less_than_int(X,Y) = 1, fraudTime_score(Card, Y).
fraudTime_score(Card, X) :- transaction(X,_,_,Card,_,_,_,_,fraud,_).
example(fraud(Card, End)) :- fraudEndTime_score(Card, End).


decreaseAtLeast(1).
decreaseAtLeast(2).
decreaseAtLeast(5).
decreaseAtLeast(10).
decreaseAtLeast(20).
decreaseAtLeast(30).
decreaseAtLeast(50).
decreaseAtLeast(70).
decreaseAtLeast(100).

increaseAtLeast(1).
increaseAtLeast(2).
increaseAtLeast(5).
increaseAtLeast(10).
increaseAtLeast(20).
increaseAtLeast(30).
increaseAtLeast(50).
increaseAtLeast(70).
increaseAtLeast(100).

totalAtLeast(1).
totalAtLeast(2).
totalAtLeast(5).
totalAtLeast(10).
totalAtLeast(20).
totalAtLeast(30).
totalAtLeast(50).
totalAtLeast(70).
totalAtLeast(100).

decreasingCountAtLeast(Card, D) :-
  decreasingCount(Card, X),
  decreaseAtLeast(D),
  X >= D,
  #false : X >= D1, decreaseAtLeast(D1), D1 > D.

increasingCountAtLeast(Card, D) :-
  increasingCount(Card, X),
  increaseAtLeast(D),
  X >= D,
  #false : X >= D1, increaseAtLeast(D1), D1 > D.

totalTransactionsCountAtLeast(Card, D) :-
  totalCount(Card, X),
  totalAtLeast(D),
  X >= D,
  #false : X >= D1, totalAtLeast(D1), D1 > D.


decreasingCount(Card, X) :- cardId(Card), X = #count {Time: transaction(Card, decreasing, Time)}.
increasingCount(Card, X) :- cardId(Card), X = #count {Time: transaction(Card, increasing, Time)}.
totalCount(Card, X) :- cardId(Card), X = #count {Time: transaction(Time,_,_,Card,_,_,_,_,_,_)}.