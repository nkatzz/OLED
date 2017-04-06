%============================================================
% This is the background knowledge used to pre-process the
% data (and generate all the auxiliary fluents etc)
%============================================================



%-----------------------
% Event Calculus Axioms
%-----------------------
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

% Basic comparison:

smaller(tiny_amount,very_small_amount).
smaller(very_small_amount,small_amount).
smaller(small_amount,medium_amount).
smaller(medium_amount,high_amount).
smaller(medium_amount,high_amount).
smaller(high_amount,very_high_amount).
smaller(very_high_amount,massive_amount).

smaller(X,Y) :- smaller(X,Z), amount(Z), smaller(Z,Y).
larger(X,Y) :- smaller(Y,X).

%--------------------------------------------------------------------------
% A transaction is represented by transaction/29:
% transaction(timestamp,transaction_id,is_cnp,amount_eur,
%              card_pan,card_exp_date,card_country,card_family,
%              card_type,card_tech,acquirer_country,merchant_mcc,
%              terminal_brand,terminal_id,terminal_type,terminal_emv,
%              transaction_response,card_auth,terminal_auth,
%              client_auth,card_band,cvv_validation,tmp_card_pan,
%              tmp_card_exp_date,transaction_type,auth_type,fraud,pattern).
%---------------------------------------------------------------------------

cardId(Card) :- transaction(_,_,_,_,Card,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).
time(T) :- transaction(T,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).
amountNum(Amount) :- transaction(_,_,_,Amount,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

% ---------------------------------
% Definition of auxiliary fluents
% ---------------------------------
fluent(amount(Card,Amount)) :- cardId(Card),amount(Amount).
fluent(amountNum(Card,Amount)) :- cardId(Card),amountNum(Amount).
fluent(amountTransition(Card,X,Y)) :- cardId(Card), amount(X), amount(Y).

fluent(cardActive(Card)) :- cardId(Card).
fluent(cardInactiveFor(Card, Count)) :- cardId(Card), cardGoesInactiveCounter(Count).

fluent(increasingSequence(Card)) :- cardId(Card).
fluent(decreasingSequence(Card)) :- cardId(Card).

%---------------------------------------------------
% Initiation and termination of the amount/2 fluent.
%---------------------------------------------------
initiatedAt(amount(Card,Amount), T) :-
  happensAt(auxAmount(Card,Amount), T).

terminatedAt(amount(Card,Amount), T) :-
  amount(Amount),amount(Amount1),
  happensAt(auxAmount(Card,Amount1), T),
  Amount != Amount1.

terminatedAt(amount(Card,Amount), T) :-
  amount(Amount),
  happensAt(disappear(Card), T).


%-------------------------------------------------------
% Initiation and termination of the amountNum/2 fluent.
% Its difference from the amount/2 fluent is that it
% carries along the precise numerical amount of a
% transaction, instead of a quantization of the amount.
% It is used in the definition of increasingSequence/1
% and decreasingSequence
%-------------------------------------------------------

initiatedAt(amountNum(Card,Amount), T) :-
  happensAt(auxAmountNum(Card,Amount), T).

terminatedAt(amountNum(Card,Amount), T) :-
  amountNum(Amount),
  happensAt(auxAmountNum(Card,Amount1), T),
  Amount != Amount1.

terminatedAt(amountNum(Card,Amount), T) :-
  amountNum(Amount),
  happensAt(disappear(Card), T).

%--------------------------------------------------------------
% Initiation and termination of the amountTransition/2 fluent.
%--------------------------------------------------------------
initiatedAt(amountTransition(Card,X,Y), T) :-
  holdsAt(amount(Card,X),T),
  happensAt(auxAmount(Card,Y), T),
  X != Y.

terminatedAt(amountTransition(Card,X,Y), T) :-
   holdsAt(amountTransition(Card,X,Y), T),
   happensAt(disappear(Card), T).


%----------------------------------------------------
% Initiation and termination of increasingSequence/1
%----------------------------------------------------

initiatedAt(increasingSequence(Card),T) :-
  holdsAt(amountNum(Card,Amount), T),
  happensAt(auxAmountNum(Card,Amount1), T),
  less_than(Amount,Amount1,1).

less_than(X,Y,@less_than(X,Y)) :- amountNum(X), amountNum(Y).

terminatedAt(increasingSequence(Card),T) :-
  holdsAt(increasingSequence(Card),T),
  holdsAt(amountNum(Card,Amount), T),
  happensAt(auxAmountNum(Card,Amount1), T),
  greater_than(Amount,Amount1,1).

greater_than(X,Y,@greater_than(X,Y)) :- amountNum(X), amountNum(Y).

terminatedAt(increasingSequence(Card),T) :-
  holdsAt(increasingSequence(Card),T),
  happensAt(disappear(Card), T).



%----------------------------------------------------
% Initiation and termination of decreasingSequence/1
%----------------------------------------------------

initiatedAt(decreasingSequence(Card),T) :-
  holdsAt(amountNum(Card,Amount), T),
  happensAt(auxAmountNum(Card,Amount1), T),
  greater_than(Amount,Amount1,1).

terminatedAt(decreasingSequence(Card),T) :-
  holdsAt(decreasingSequence(Card),T),
  holdsAt(amountNum(Card,Amount), T),
  happensAt(auxAmountNum(Card,Amount1), T),
  less_than(Amount,Amount1,1).

terminatedAt(decreasingSequence(Card),T) :-
  holdsAt(decreasingSequence(Card),T),
  happensAt(disappear(Card), T).




%==============================
% Various auxiliary predicates
%==============================

% Helper predicates to extract the amount from a record
happensAt(auxAmount(Card, @quantize_amount(Amount)), T) :-
  transaction(T,_,_,Amount,Card,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

happensAt(auxAmountNum(Card, Amount), T) :-
  transaction(T,_,_,Amount,Card,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

% Helper predicate to capture when a card goes inactive, so that
% anything we 'remember' for that card up to that point may be discarded.
% We'll assume that a card goes inactive if it does not appear for 3
% time points (after it has appeared once). This is controlled by the
% cardGoesInactive/1 predicate.

cardGoesInactive(1).
cardGoesInactiveCounter(1..X) :- cardGoesInactive(X).

happensAt(disappear(Card), T) :-
  terminatedAt(cardActive(Card), T).

initiatedAt(cardActive(Card), T) :-
  transaction(T,_,_,_,Card,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

terminatedAt(cardActive(Card), T) :-
  cardGoesInactive(Z),
  initiatedAt(cardInactiveFor(Card, Z), T).

initiatedAt(cardInactiveFor(Card, 1), T) :-
  holdsAt(cardActive(Card), T),
  not transaction(T,_,_,_,Card,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

initiatedAt(cardInactiveFor(Card, X+1), T) :-
  holdsAt(cardInactiveFor(Card, X), T),
  not transaction(T,_,_,_,Card,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

terminatedAt(cardInactiveFor(Card, X-1), T) :-
  initiatedAt(cardInactiveFor(Card, X), T).

terminatedAt(cardInactiveFor(Card, X), T) :-
  cardGoesInactiveCounter(X),
  terminatedAt(cardActive(Card), T).


% Generate the supervision (note the fraud/1 is not declared as a fluent)

holdsAt(fraud(Card),T) :- transaction(T,_,_,_,Card,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,1,_).


#show.

#show holdsAt(increasingSequence(Card),T) : holdsAt(increasingSequence(Card),T), time(T).
#show initiatedAt(increasingSequence(Card),T) : initiatedAt(increasingSequence(Card),T), time(T).
#show terminatedAt(increasingSequence(Card),T) : terminatedAt(increasingSequence(Card),T), time(T).

#show holdsAt(decreasingSequence(Card),T) : holdsAt(decreasingSequence(Card),T), time(T).
#show initiatedAt(decreasingSequence(Card),T) : initiatedAt(decreasingSequence(Card),T), time(T).
#show terminatedAt(decreasingSequence(Card),T) : terminatedAt(decreasingSequence(Card),T), time(T).

#show holdsAt(amountTransition(Card,X,Y), T) : holdsAt(amountTransition(Card,X,Y), T), time(T).
#show initiatedAt(amountTransition(Card,X,Y), T) : initiatedAt(amountTransition(Card,X,Y), T), time(T).
#show terminatedAt(amountTransition(Card,X,Y), T) : terminatedAt(amountTransition(Card,X,Y), T), time(T).

#show happensAt(disappear(Card), T) : happensAt(disappear(Card), T), time(T).

%#show holdsAt(fraud(Card),T) : holdsAt(fraud(Card),T), time(T).


