holdsAt(F,Te) :-
       fluent(F),
       not sdFluent(F),
       initiatedAt(F,Ts),
       Te = Ts + 1,
       time(Ts),time(Te).

holdsAt(F,Te) :-
      fluent(F),
      not sdFluent(F),
      holdsAt(F,Ts),
      not terminatedAt(F,Ts),
      Te = Ts + 1,
      time(Ts),time(Te).

holdsAt(F,T) :-
       initialTime(T),
       example(holdsAt(F,T)).

initialTime(X) :- time(X), #false : X > Y, time(Y).

fluent(growing(X)) :- community(X).
fluent(shrinking(X)) :- community(X).
fluent(continuing(X)) :- community(X).
fluent(dissolving(X)) :- community(X).

community(X) :- happensAt(size(X,_),_).
community(X) :- happensAt(density(X,_),_).
community(X) :- happensAt(cohesion(X,_),_).
community(X) :- happensAt(association(X,_),_).
community(X) :- happensAt(ratio_association(X,_),_).
community(X) :- happensAt(ratio_cut(X,_),_).
community(X) :- happensAt(edge_number(X,_),_).
community(X) :- happensAt(average_path_length(X,_),_).
community(X) :- happensAt(diameter(X,_),_).
community(X) :- happensAt(clustering_coefficient(X,_),_).
community(X) :- happensAt(closeness_centrality(X,_),_).
community(X) :- happensAt(betweenness_centrality(X,_),_).
community(X) :- happensAt(eigenvector_centrality(X,_),_).
community(X) :- happensAt(centrality(X,_),_).
%*
value(X) :- happensAt(size(_,X),_).
value(X) :- happensAt(density(_,X),_).
value(X) :- happensAt(cohesion(_,X),_).
value(X) :- happensAt(association(_,X),_).
value(X) :- happensAt(ratio_association(_,X),_).
value(X) :- happensAt(ratio_cut(_,X),_).
value(X) :- happensAt(edge_number(_,X),_).
value(X) :- happensAt(average_path_length(_,X),_).
value(X) :- happensAt(diameter(_,X),_).
value(X) :- happensAt(clustering_coefficient(_,X),_).
value(X) :- happensAt(closeness_centrality(_,X),_).
value(X) :- happensAt(betweenness_centrality(_,X),_).
value(X) :- happensAt(eigenvector_centrality(_,X),_).
value(X) :- happensAt(centrality(_,X),_).
*%

value(1..5).

time(X) :- happensAt(size(_,_),X).
time(X) :- happensAt(density(_,_),X).
time(X) :- happensAt(cohesion(_,_),X).
time(X) :- happensAt(association(_,_),X).
time(X) :- happensAt(ratio_association(_,_),X).
time(X) :- happensAt(ratio_cut(_,_),X).
time(X) :- happensAt(edge_number(_,_),X).
time(X) :- happensAt(average_path_length(_,_),X).
time(X) :- happensAt(diameter(_,_),X).
time(X) :- happensAt(clustering_coefficient(_,_),X).
time(X) :- happensAt(closeness_centrality(_,_),X).
time(X) :- happensAt(betweenness_centrality(_,_),X).
time(X) :- happensAt(eigenvector_centrality(_,_),X).
time(X) :- happensAt(centrality(_,_),X).

ge(X,Y) :- X > Y, value(X), value(Y).
leq(X,Y) :- X <= Y, value(X), value(Y).

%#show.
%#show value/1.