incSeqs(1..9).
decSeqs(1..9).

cardId(Card) :-transaction(_,_,_,Card,_,_,_,_,_,_).

example(fraud(Card)) :- transaction(_,_,_,Card,_,_,_,_,fraud,_).
