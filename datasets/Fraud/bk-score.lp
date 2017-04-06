incSeqs(1..9).
decSeqs(1..9).

cardId(Card) :-transaction(_,_,_,Card,_,_,_,_,_,_).

example(fraud(Card)) :- transaction(_,_,_,Card,_,_,_,_,fraud,_).

tps(I, X) :- rule(I), X = #count {A: example( fraud(A) ), marked(I, fraud(A) ) }.
fps(I, X) :- rule(I), X = #count {A: not example( fraud(A) ), marked(I, fraud(A) ) }.
fns(I, X) :- rule(I), X = #count {A: example( fraud(A) ), not marked(I, fraud(A) ) }.









