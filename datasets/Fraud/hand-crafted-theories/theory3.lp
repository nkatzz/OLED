% tps: 313
% fps: 41
% fns: 94
% precision: 0.8841808
% recall: 0.7690418
% f-score: 0.8226018
% training time: 13091.704399309
% Theory found:

fraud(X0,X1) :- 
      fraudWithin(X2,X1,4),
      transaction(X0,increasing,X1),
      transaction(X0,fraud_start,X2),
      before(X2,X1),
      increasingCountAtLeast(X0,5),
      medium_amount(X2).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,5),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,5).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      increasingCountAtLeast(X0,1),
      massive_amount(X1),
      high_amount(X2),
      diffAcquirer(X0,X1).
fraud(X0,X1) :- 
      fraudWithin(X7,X1,2),
      transaction(X0,increasing,X1),
      decreasingCountAtLeast(X0,2),
      totalTransactionsCountAtLeast(X0,5),
      very_small_amount(X5),
      medium_amount(X1),
      increasingCountAtLeast(X0,2),
      transaction(X0,decreasing,X5),
      before(X5,X1),
      very_high_amount(X7).
fraud(X0,X1) :- 
      fraudWithin(X3,X1,2),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,5).
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      transaction(X0,decreasing,X2),
      before(X2,X1),
      transaction(X0,decreasing,X5),
      before(X5,X2),
      very_small_amount(X2),
      transaction(X0,increasing,X7),
      before(X7,X5),
      fraudWithin(X8,X1,1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X3,X1,2),
      massive_amount(X1),
      medium_amount(X4),
      totalTransactionsCountAtLeast(X0,5),
      transaction(X0,decreasing,X4),
      before(X4,X1),
      decreasingCountAtLeast(X0,2),
      transaction(X0,decreasing,X7),
      before(X4,X7),
      massive_amount(X3),
      medium_amount(X7).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,1),
      massive_amount(X1),
      totalTransactionsCountAtLeast(X0,5),
      very_high_amount(X2),
      decreasingCountAtLeast(X0,2),
      increasingCountAtLeast(X0,5).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      increasingCountAtLeast(X0,1),
      massive_amount(X1),
      totalTransactionsCountAtLeast(X0,2).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,4),
      massive_amount(X1),
      tiny_amount(X2),
      cardId(X0).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,4),
      transaction(X0,fraud_start,X2),
      before(X2,X1),
      massive_amount(X1),
      increasingCountAtLeast(X0,1),
      very_high_amount(X2),
      totalTransactionsCountAtLeast(X0,2).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,5),
      decreasingCountAtLeast(X0,5),
      high_amount(X1).
fraud(X0,X1) :- 
      tiny_amount(X2),
      fraudWithin(X2,X1,1),
      massive_amount(X1),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,1).
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      fraudWithin(X3,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      increasingCountAtLeast(X0,2),
      high_amount(X3),
      decreasingCountAtLeast(X0,2).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,7),
      decreasingCountAtLeast(X0,5),
      high_amount(X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      decreasingCountAtLeast(X0,2),
      massive_amount(X1),
      increasingCountAtLeast(X0,2),
      medium_amount(X2).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,4),
      decreasingCountAtLeast(X0,5),
      high_amount(X1).
fraud(X0,X1) :- 
      increasingCountAtLeast(X0,5),
      very_high_amount(X1),
      medium_amount(X4),
      high_amount(X7),
      fraudWithin(X2,X1,3),
      transaction(X0,increasing,X1).
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      massive_amount(X1),
      decreasingCountAtLeast(X0,1),
      massive_amount(X2),
      transaction(X0,fraud_start,X2),
      before(X2,X1),
      diffAcquirer(X0,X1).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,3),
      massive_amount(X1),
      tiny_amount(X2),
      cardId(X0).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,7),
      increasingCountAtLeast(X0,5),
      transaction(X0,increasing,X1).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,3),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,1),
      massive_amount(X1),
      very_high_amount(X2),
      totalTransactionsCountAtLeast(X0,2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X3,X1,2),
      decreasingCountAtLeast(X0,2),
      very_small_amount(X3),
      massive_amount(X1).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,15),
      medium_amount(X2),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,5),
      very_high_amount(X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      high_amount(X1),
      decreasingCountAtLeast(X0,2),
      medium_amount(X2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      massive_amount(X1),
      fraudWithin(X2,X1,1),
      totalTransactionsCountAtLeast(X0,5),
      high_amount(X2),
      decreasingCountAtLeast(X0,2),
      high_amount(X5),
      medium_amount(X4),
      transaction(X0,increasing,X5),
      before(X5,X1).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,4),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,1),
      totalTransactionsCountAtLeast(X0,2),
      massive_amount(X1),
      small_amount(X2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      very_high_amount(X2),
      massive_amount(X1),
      increasingCountAtLeast(X0,2),
      decreasingCountAtLeast(X0,2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      decreasingCountAtLeast(X0,2),
      increasingCountAtLeast(X0,2),
      very_high_amount(X5),
      transaction(X0,increasing,X5),
      before(X5,X1),
      very_small_amount(X2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,4),
      increasingCountAtLeast(X0,1),
      medium_amount(X2),
      massive_amount(X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,5),
      massive_amount(X1),
      fraudWithin(X3,X1,1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      increasingCountAtLeast(X0,1),
      massive_amount(X1),
      medium_amount(X2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      transaction(X0,increasing,X4),
      before(X4,X1),
      fraudWithin(X2,X1,1),
      decreasingCountAtLeast(X0,2),
      massive_amount(X1),
      massive_amount(X4),
      medium_amount(X2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      transaction(X0,increasing,X5),
      before(X5,X1),
      fraudWithin(X2,X1,1),
      decreasingCountAtLeast(X0,2),
      medium_amount(X5),
      medium_amount(X2).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X3,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      high_amount(X1),
      decreasingCountAtLeast(X0,2),
      small_amount(X3).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,4),
      massive_amount(X1),
      very_small_amount(X2),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,1).
fraud(X0,X1) :- 
      fraudWithin(X6,X1,2),
      transaction(X0,increasing,X1),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      very_small_amount(X2),
      increasingCountAtLeast(X0,2),
      transaction(X0,decreasing,X2),
      before(X2,X1),
      very_high_amount(X6),
      transaction(X0,decreasing,X7),
      before(X2,X7),
      high_amount(X4),
      transaction(X0,increasing,X4),
      before(X4,X7).
fraud(X0,X1) :- 
      fraudWithin(X4,X1,2),
      massive_amount(X4),
      transaction(X0,increasing,X1),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      increasingCountAtLeast(X0,2),
      small_amount(X8),
      transaction(X0,decreasing,X8),
      before(X8,X1),
      small_amount(X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      transaction(X0,increasing,X5),
      before(X5,X1),
      fraudWithin(X2,X1,1),
      decreasingCountAtLeast(X0,2),
      very_high_amount(X5),
      transaction(X0,decreasing,X6),
      before(X5,X6).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      medium_amount(X1),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      massive_amount(X2),
      increasingCountAtLeast(X0,2),
      very_high_amount(X8),
      transaction(X0,decreasing,X8),
      before(X8,X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      decreasingCountAtLeast(X0,2),
      medium_amount(X4),
      high_amount(X5),
      high_amount(X2),
      increasingCountAtLeast(X0,2),
      transaction(X0,decreasing,X4),
      before(X4,X1),
      transaction(X0,increasing,X5),
      before(X5,X4).
fraud(X0,X1) :- 
      fraudWithin(X7,X1,2),
      transaction(X0,increasing,X1),
      decreasingCountAtLeast(X0,2),
      massive_amount(X1),
      increasingCountAtLeast(X0,2),
      massive_amount(X7).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      transaction(X0,increasing,X5),
      before(X5,X1),
      decreasingCountAtLeast(X0,2),
      fraudWithin(X2,X1,1),
      very_high_amount(X2),
      massive_amount(X5),
      high_amount(X8).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X3,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      very_high_amount(X1),
      medium_amount(X3),
      decreasingCountAtLeast(X0,2),
      very_high_amount(X8),
      transaction(X0,decreasing,X8),
      before(X8,X1),
      transaction(X0,decreasing,X6),
      before(X8,X6).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,3),
      massive_amount(X1),
      massive_amount(X2),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,1),
      diffAcquirer(X0,X1),
      totalTransactionsCountAtLeast(X0,2),
      transaction(X0,fraud_start,X2),
      before(X2,X1).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,2),
      transaction(X0,increasing,X1),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      very_high_amount(X1),
      medium_amount(X4),
      very_high_amount(X2),
      transaction(X0,decreasing,X4),
      before(X4,X1),
      very_high_amount(X8),
      increasingCountAtLeast(X0,2),
      high_amount(X5),
      transaction(X0,decreasing,X3),
      before(X4,X3),
      very_high_amount(X7),
      medium_amount(X3).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X4,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      increasingCountAtLeast(X0,2),
      high_amount(X4),
      small_amount(X7),
      transaction(X0,decreasing,X7),
      before(X7,X1),
      very_small_amount(X2),
      transaction(X0,decreasing,X2),
      before(X7,X2),
      high_amount(X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      massive_amount(X1),
      increasingCountAtLeast(X0,1),
      small_amount(X2).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,4),
      transaction(X0,increasing,X1),
      increasingCountAtLeast(X0,1),
      massive_amount(X1),
      high_amount(X2).
fraud(X0,X1) :- 
      increasingCountAtLeast(X0,5),
      totalTransactionsCountAtLeast(X0,5),
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      small_amount(X2),
      decreasingCountAtLeast(X0,2),
      very_high_amount(X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      massive_amount(X1),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      small_amount(X7),
      massive_amount(X2),
      transaction(X0,decreasing,X7),
      before(X7,X1),
      transaction(X0,decreasing,X6),
      before(X6,X7),
      increasingCountAtLeast(X0,2),
      high_amount(X6).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X3,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      very_high_amount(X1),
      very_high_amount(X3),
      very_small_amount(X2),
      increasingCountAtLeast(X0,2),
      transaction(X0,decreasing,X2),
      before(X2,X1),
      transaction(X0,decreasing,X6),
      before(X2,X6),
      transaction(X0,increasing,X7),
      before(X7,X6).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      massive_amount(X1),
      massive_amount(X4),
      decreasingCountAtLeast(X0,2),
      transaction(X0,increasing,X4),
      before(X4,X1).
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X4,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      medium_amount(X8),
      small_amount(X4),
      massive_amount(X1),
      very_high_amount(X6),
      transaction(X0,decreasing,X8),
      before(X8,X1).
fraud(X0,X1) :- 
      fraudWithin(X2,X1,3),
      transaction(X0,increasing,X1),
      very_small_amount(X2),
      massive_amount(X1),
      increasingCountAtLeast(X0,1).

