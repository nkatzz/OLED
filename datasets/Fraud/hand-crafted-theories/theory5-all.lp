fraud(X0) :-
      largestIncreasingSeqAtLeast(X0,4).

fraud(X0) :-
      largestIncreasingSeqAtLeast(X0,5),
      decreasingSeqsAtLeast(X0,0).

fraud(X0) :-
      decreasingSeqsAtLeast(X0,4),
      largestDecreasingSeqAtLeast(X0,4).

fraud(X0) :-
      decreasingSeqsAtLeast(X0,3),
      increasingSeqsAtLeast(X0,3),
      largestDecreasingSeqAtLeast(X0,6).

fraud(X0) :-
      largestDecreasingSeqAtLeast(X0,9).

fraud(X0) :-
      largestDecreasingSeqAtLeast(X0,5),
      largestIncreasingSeqAtLeast(X0,4).

fraud(X0) :-
      largestDecreasingSeqAtLeast(X0,3).

fraud(X0) :-
      decreasingSeqsAtLeast(X0,10),
      largestDecreasingSeqAtLeast(X0,4),
      largestIncreasingSeqAtLeast(X0,3).