% score (precision): 0.34983763098716736, tps: 10881, fps: 20222, fns: 59600
%fraud(X0,X1) :- 
%      transaction(X0,increasing,X1),
%      massive_amount(X1),
%      increasingCountAtLeast(X0,1),
%      fraudWithin(X2,X1,1).

% score (precision): 0.9997676014900208, tps: 8605, fps: 2, fns: 61884
fraud(X0,X1) :- 
      decreasingCountAtLeast(X0,5),
      fraudWithin(X2,X1,5),
      transaction(X0,fraud_start,X2),
      before(X2,X1),
      high_amount(X1).

% score (precision): 1.0, tps: 148, fps: 0, fns: 54361
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      transaction(X0,decreasing,X5),
      before(X5,X1),
      transaction(X0,decreasing,X3),
      before(X3,X5),
      fraudWithin(X2,X1,1),
      increasingCountAtLeast(X0,2),
      medium_amount(X5),
      very_small_amount(X1).

% score (precision): 0.9987195134162903, tps: 12479, fps: 16, fns: 49543
fraud(X0,X1) :- 
      increasingCountAtLeast(X0,5),
      very_high_amount(X1),
      fraudWithin(X2,X1,7),
      transaction(X0,increasing,X1).

% score (precision): 0.9971299767494202, tps: 7296, fps: 21, fns: 54147
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      tiny_amount(X2),
      fraudWithin(X2,X1,2),
      increasingCountAtLeast(X0,1),
      massive_amount(X1).

% score (precision): 0.26338329911231995, tps: 984, fps: 2752, fns: 53095
%fraud(X0,X1) :- 
%      transaction(X0,decreasing,X1),
%      massive_amount(X1),
%      decreasingCountAtLeast(X0,1),
%      diffAcquirer(X0,X1),
%      totalTransactionsCountAtLeast(X0,2).

% score (precision): 1.0, tps: 97, fps: 0, fns: 42750
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      massive_amount(X1),
      fraudWithin(X2,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      transaction(X0,increasing,X7),
      before(X7,X1),
      transaction(X0,increasing,X4),
      before(X4,X7),
      transaction(X0,decreasing,X3),
      before(X3,X4),
      high_amount(X3),
      massive_amount(X4).

% score (precision): 0.9847789406776428, tps: 4076, fps: 63, fns: 51238
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      transaction(X0,increasing,X7),
      before(X7,X1),
      transaction(X0,increasing,X4),
      before(X4,X7),
      transaction(X0,decreasing,X3),
      before(X3,X4).

%score (precision): 0.5298296213150024, tps: 20213, fps: 17937, fns: 33003
%fraud(X0,X1) :- 
%      transaction(X0,increasing,X1),
%      fraudWithin(X2,X1,4),
%      massive_amount(X1),
%      increasingCountAtLeast(X0,1),
%      totalTransactionsCountAtLeast(X0,2),
%      transaction(X0,fraud_start,X2),
%      before(X2,X1).

% score (precision): 1.0, tps: 85, fps: 0, fns: 42974
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      totalTransactionsCountAtLeast(X0,5),
      very_high_amount(X1),
      transaction(X0,increasing,X8),
      before(X8,X1),
      transaction(X0,increasing,X5),
      before(X5,X8),
      decreasingCountAtLeast(X0,2),
      transaction(X0,decreasing,X4),
      before(X4,X5),
      massive_amount(X5),
      transaction(X0,decreasing,X3),
      before(X3,X4).

% score (precision): 0.9996660947799683, tps: 8981, fps: 3, fns: 35526
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,15),
      increasingCountAtLeast(X0,5),
      very_high_amount(X1),
      medium_amount(X2).

% score (precision): 1.0, tps: 5602, fps: 0, fns: 23244
fraud(X0,X1) :- 
      decreasingCountAtLeast(X0,5),
      fraudWithin(X2,X1,7),
      transaction(X0,decreasing,X1),
      high_amount(X1),
      massive_amount(X2).

% score (precision): 1.0, tps: 185, fps: 0, fns: 50562
fraud(X0,X1) :- 
      increasingCountAtLeast(X0,5),
      fraudWithin(X2,X1,2),
      transaction(X0,decreasing,X1).

% score (precision): 0.875, tps: 252, fps: 36, fns: 41265
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      transaction(X0,decreasing,X3),
      before(X3,X1),
      fraudWithin(X2,X1,2),
      increasingCountAtLeast(X0,2),
      very_small_amount(X3),
      small_amount(X1).

% score (precision): 0.9780219793319702, tps: 89, fps: 2, fns: 35595
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      transaction(X0,increasing,X7),
      before(X7,X1),
      transaction(X0,increasing,X4),
      before(X4,X7),
      massive_amount(X7),
      transaction(X0,decreasing,X3),
      before(X3,X4),
      very_small_amount(X3),
      massive_amount(X4).

% score (precision): 1.0, tps: 72, fps: 0, fns: 32483
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      totalTransactionsCountAtLeast(X0,5),
      transaction(X0,increasing,X5),
      before(X5,X1),
      transaction(X0,increasing,X4),
      before(X4,X5),
      transaction(X0,decreasing,X3),
      before(X3,X4),
      high_amount(X3),
      decreasingCountAtLeast(X0,2),
      very_high_amount(X4),
      very_high_amount(X2).

% score (precision): 0.9976958632469177, tps: 433, fps: 1, fns: 28849
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,1),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      transaction(X0,increasing,X7),
      before(X7,X1),
      transaction(X0,increasing,X3),
      before(X3,X7),
      very_high_amount(X3).

% score (precision): 0.982758641242981, tps: 57, fps: 1, fns: 5881
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      transaction(X0,decreasing,X6),
      before(X6,X1),
      fraudWithin(X2,X1,1),
      increasingCountAtLeast(X0,2),
      medium_amount(X6),
      transaction(X0,increasing,X5),
      before(X5,X6),
      transaction(X0,increasing,X4),
      before(X4,X5).

% score (precision): 1.0, tps: 113, fps: 0, fns: 32602
fraud(X0,X1) :- 
      decreasingCountAtLeast(X0,5),
      fraudWithin(X2,X1,2),
      transaction(X0,increasing,X1).

% score (precision): 1.0, tps: 83, fps: 0, fns: 23938
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      massive_amount(X1),
      fraudWithin(X2,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      transaction(X0,increasing,X4),
      before(X4,X1),
      transaction(X0,increasing,X3),
      before(X3,X4),
      small_amount(X2).

% score (precision): 0.9840425252914429, tps: 185, fps: 3, fns: 19279
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,2),
      totalTransactionsCountAtLeast(X0,5),
      decreasingCountAtLeast(X0,2),
      transaction(X0,increasing,X5),
      before(X5,X1),
      transaction(X0,increasing,X3),
      before(X3,X5),
      very_high_amount(X3),
      high_amount(X2).

%score (precision): 0.6854838728904724, tps: 85, fps: 39, fns: 23595
%fraud(X0,X1) :- 
%      transaction(X0,increasing,X1),
%      fraudWithin(X2,X1,3),
%      totalTransactionsCountAtLeast(X0,5),
%      decreasingCountAtLeast(X0,2),
%      small_amount(X2),
%      massive_amount(X1).

% score (precision): 1.0, tps: 174, fps: 0, fns: 25864
fraud(X0,X1) :- 
      increasingCountAtLeast(X0,5),
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,1).

% score (precision): 1.0, tps: 94, fps: 0, fns: 12896
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      transaction(X0,decreasing,X4),
      before(X4,X1),
      transaction(X0,decreasing,X3),
      before(X3,X4),
      fraudWithin(X2,X1,2),
      increasingCountAtLeast(X0,2),
      medium_amount(X3),
      small_amount(X1).

% score (precision): 1.0, tps: 63, fps: 0, fns: 16926
fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      fraudWithin(X2,X1,3),
      decreasingCountAtLeast(X0,5).

% score (precision): 0.9984931945800781, tps: 1988, fps: 3, fns: 7803
fraud(X0,X1) :- 
      decreasingCountAtLeast(X0,5),
      fraudWithin(X2,X1,15),
      transaction(X0,decreasing,X1),
      high_amount(X1).

% score (precision): 0.73046875, tps: 187, fps: 69, fns: 5239
fraud(X0,X1) :- 
      transaction(X0,decreasing,X1),
      transaction(X0,decreasing,X3),
      before(X3,X1),
      high_amount(X3),
      totalTransactionsCountAtLeast(X0,5),
      fraudWithin(X2,X1,3),
      increasingCountAtLeast(X0,2),
      transaction(X0,increasing,X8),
      before(X3,X8).

fraud(X0,X1) :- 
      transaction(X0,increasing,X1),
      massive_amount(X1),
      fraudWithin(X2,X1,2).


