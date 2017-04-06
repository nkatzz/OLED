fraud(X0) :-
      decreasingSeqsAtLeast(X0,4),
      largestDecreasingSeqAtLeast(X0,4).

fraud(X0) :-
      largestDecreasingSeqAtLeast(X0,5),
      largestIncreasingSeqAtLeast(X0,4).

fraud(X0) :-
      largestDecreasingSeqAtLeast(X0,4),
      largestIncreasingSeqAtLeast(X0,4).