%*
This theory achieves the following on the entire synthetic dataset:
tps: 32281
fps: 1704
fns: 20459
precision: 0.9498602
recall: 0.61207813
f-score: 0.7444451
training time: 0.0
*%

fraud(X0,X1) :-
      transaction(X0,high_amount,X6),
      before(X6,X1),
      transaction(X0,high_amount,X3),
      before(X3,X6),
      transaction(X0,very_high_amount,X7),
      before(X7,X3),
      transaction(X0,very_high_amount,X5),
      before(X5,X7),
      transaction(X0,massive_amount,X2),
      before(X2,X5),
      transaction(X0,high_amount,X1),
      before(X2,X1).
fraud(X0,X1) :-
      transaction(X0,tiny_amount,X2),
      before(X2,X1),
      transaction(X0,massive_amount,X1).
fraud(X0,X1) :-
      transaction(X0,massive_amount,X8),
      before(X8,X1),
      transaction(X0,very_small_amount,X2),
      before(X2,X8),
      transaction(X0,medium_amount,X1),
      before(X2,X1),
      transaction(X0,medium_amount,X3),
      before(X3,X1),
      transaction(X0,very_high_amount,X4),
      before(X4,X3),
      transaction(X0,very_small_amount,X7),
      before(X4,X7),
      transaction(X0,very_high_amount,X6),
      before(X7,X6),
      transaction(X0,medium_amount,X5),
      before(X6,X5).
fraud(X0,X1) :-
      transaction(X0,very_high_amount,X1),
      transaction(X0,high_amount,X4),
      before(X4,X1),
      transaction(X0,high_amount,X5),
      before(X5,X4),
      transaction(X0,medium_amount,X8),
      before(X8,X5),
      transaction(X0,medium_amount,X7),
      before(X7,X8).
fraud(X0,X1) :-
      transaction(X0,very_high_amount,X1),
      transaction(X0,very_small_amount,X7),
      before(X7,X1),
      transaction(X0,high_amount,X3),
      before(X3,X7),
      transaction(X0,massive_amount,X4),
      before(X4,X3),
      transaction(X0,massive_amount,X2),
      before(X4,X2),
      transaction(X0,medium_amount,X5),
      before(X5,X2),
      transaction(X0,medium_amount,X8),
      before(X5,X8),
      transaction(X0,medium_amount,X6),
      before(X8,X6).
fraud(X0,X1) :-
      transaction(X0,medium_amount,X5),
      before(X5,X1),
      transaction(X0,very_small_amount,X8),
      before(X8,X5),
      transaction(X0,small_amount,X3),
      before(X3,X8),
      transaction(X0,massive_amount,X2),
      before(X3,X2),
      transaction(X0,high_amount,X1),
      before(X2,X1),
      transaction(X0,very_high_amount,X4),
      before(X4,X1),
      transaction(X0,medium_amount,X6),
      before(X6,X4).
fraud(X0,X1) :-
      transaction(X0,small_amount,X1),
      transaction(X0,massive_amount,X6),
      before(X6,X1),
      transaction(X0,medium_amount,X2),
      before(X6,X2),
      transaction(X0,high_amount,X3),
      before(X3,X2),
      transaction(X0,high_amount,X7),
      before(X3,X7),
      transaction(X0,medium_amount,X8),
      before(X7,X8),
      transaction(X0,massive_amount,X5),
      before(X8,X5),
      transaction(X0,very_small_amount,X4),
      before(X4,X5).
fraud(X0,X1) :-
      transaction(X0,medium_amount,X5),
      before(X5,X1),
      transaction(X0,very_high_amount,X3),
      before(X3,X5),
      transaction(X0,very_high_amount,X4),
      before(X4,X3),
      transaction(X0,high_amount,X8),
      before(X4,X8),
      transaction(X0,high_amount,X1),
      before(X8,X1),
      transaction(X0,massive_amount,X7),
      before(X7,X1),
      transaction(X0,high_amount,X2),
      before(X7,X2).
fraud(X0,X1) :-
      transaction(X0,medium_amount,X4),
      before(X4,X1),
      transaction(X0,small_amount,X8),
      before(X8,X4),
      transaction(X0,very_small_amount,X5),
      before(X8,X5),
      transaction(X0,high_amount,X1),
      before(X5,X1),
      transaction(X0,high_amount,X6),
      before(X6,X1),
      transaction(X0,medium_amount,X2),
      before(X6,X2),
      transaction(X0,very_small_amount,X3),
      before(X3,X2),
      transaction(X0,very_small_amount,X7),
      before(X3,X7).
fraud(X0,X1) :-
      transaction(X0,very_high_amount,X3),
      before(X3,X1),
      transaction(X0,very_small_amount,X6),
      before(X6,X3),
      transaction(X0,very_high_amount,X1),
      before(X6,X1),
      transaction(X0,massive_amount,X4),
      before(X4,X1),
      transaction(X0,high_amount,X5),
      before(X4,X5),
      transaction(X0,massive_amount,X2),
      before(X5,X2),
      transaction(X0,high_amount,X8),
      before(X8,X2).
fraud(X0,X1) :-
      transaction(X0,very_high_amount,X2),
      before(X2,X1),
      transaction(X0,very_high_amount,X5),
      before(X5,X2),
      transaction(X0,massive_amount,X7),
      before(X7,X5),
      transaction(X0,high_amount,X1),
      before(X7,X1),
      transaction(X0,medium_amount,X3),
      before(X3,X1),
      transaction(X0,massive_amount,X8),
      before(X3,X8).
fraud(X0,X1) :-
      transaction(X0,small_amount,X1),
      transaction(X0,medium_amount,X6),
      before(X6,X1),
      transaction(X0,massive_amount,X5),
      before(X5,X6),
      transaction(X0,high_amount,X4),
      before(X4,X5).
