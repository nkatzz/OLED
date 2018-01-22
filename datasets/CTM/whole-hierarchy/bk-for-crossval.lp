
holdsAt(F,Te) :-
       initiatedAt(F,Ts),
       Te = Ts + 1,
       ttime(Ts),ttime(Te).

holdsAt(F,Te) :-
       holdsAt(F,Ts),
       not terminatedAt(F,Ts),
       Te = Ts + 1,
       ttime(Ts),ttime(Te).

holdsAt(F,T) :-
       initialTime(T),
       example(holdsAt(F,T)).


initialTime(X) :- ttime(X), #false : X > Y, ttime(Y).
 
% Extra Background knowledge start:
%*
terminatedAt(sharp_turn(X2,X1,sharp),X3) :- 
        happensAt(sharp_turn_end(X2,X1,sharp),X3).
terminatedAt(sharp_turn(X3,X1,very_sharp),X2) :- 
        happensAt(sharp_turn_end(X3,X1,very_sharp),X2).
initiatedAt(sharp_turn(X2,X1,sharp),X3) :- 
        happensAt(sharp_turn_start(X2,X1,sharp),X3).
initiatedAt(sharp_turn(X3,X1,very_sharp),X2) :- 
        happensAt(sharp_turn_start(X3,X1,very_sharp),X2).


terminatedAt(abrupt_acceleration(X2,X1,abrupt),X3) :- 
        happensAt(abrupt_acceleration_end(X2,X1,abrupt),X3).
terminatedAt(abrupt_acceleration(X2,X1,very_abrupt),X3) :- 
        happensAt(abrupt_acceleration_end(X2,X1,very_abrupt),X3).
initiatedAt(abrupt_acceleration(X2,X1,abrupt),X3) :- 
        happensAt(abrupt_acceleration_start(X2,X1,abrupt),X3).
initiatedAt(abrupt_acceleration(X3,X1,very_abrupt),X2) :- 
        happensAt(abrupt_acceleration_start(X3,X1,very_abrupt),X2).


terminatedAt(abrupt_deceleration(X3,X1,abrupt),X2) :- 
        happensAt(abrupt_deceleration_end(X3,X1,abrupt),X2).
terminatedAt(abrupt_deceleration(X2,X1,very_abrupt),X3) :- 
        happensAt(abrupt_deceleration_end(X2,X1,very_abrupt),X3).
initiatedAt(abrupt_deceleration(X3,X1,abrupt),X2) :- 
        happensAt(abrupt_deceleration_start(X3,X1,abrupt),X2).
initiatedAt(abrupt_deceleration(X3,X1,very_abrupt),X2) :- 
        happensAt(abrupt_deceleration_start(X3,X1,very_abrupt),X2).
*%
% Extra Background knowledge end








vehicleId(VehicleId):-
        happensAt(abrupt_acceleration_start(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(abrupt_acceleration_end(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(abrupt_deceleration_start(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(abrupt_deceleration_end(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(sharp_turn_start(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(sharp_turn_end(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(internal_temperature_change(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(noise_level_change(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(passenger_density_change(VehicleId,_,_),_).
vehicleId(VehicleId):-
        happensAt(stop_enter(VehicleId,_,_,_),_).
vehicleId(VehicleId):-
        happensAt(stop_leave(VehicleId,_,_,_),_).

vehicleType(VehicleType):-
        happensAt(abrupt_acceleration_start(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(abrupt_acceleration_end(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(abrupt_deceleration_start(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(abrupt_deceleration_end(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(sharp_turn_start(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(sharp_turn_end(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(internal_temperature_change(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(noise_level_change(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(passenger_density_change(_,VehicleType,_),_).
vehicleType(VehicleType):-
        happensAt(stop_enter(_,VehicleType,_,_),_).
vehicleType(VehicleType):-
        happensAt(stop_leave(_,VehicleType,_,_),_).

evalue(V):-
        happensAt(abrupt_acceleration_start(_,_,V),_).
evalue(V):-
        happensAt(abrupt_acceleration_end(_,_,V),_).
evalue(V):-
        happensAt(abrupt_deceleration_start(_,_,V),_).
evalue(V):-
        happensAt(abrupt_deceleration_end(_,_,V),_).
evalue(V):-
        happensAt(internal_temperature_change(_,_,V),_).
evalue(V):-
        happensAt(noise_level_change(_,_,V),_).
evalue(V):-
        happensAt(passenger_density_change(_,_,V),_).
evalue(V):-
        happensAt(sharp_turn_start(_,_,V),_).
evalue(V):-
        happensAt(sharp_turn_end(_,_,V),_).
evalue(V):-
        happensAt(stop_enter(_,_,_,V),_).
evalue(V):-
        happensAt(stop_leave(_,_,_,V),_).


happensAt(my_stop_enter(VId,VType,stopId,Value),T):-
        happensAt(stop_enter(VId,VType,_,Value),T). 

happensAt(my_stop_leave(VId,VType,stopId,Value),T):-
        happensAt(stop_leave(VId,VType,_,Value),T).


stopId(S):-
        happensAt(my_stop_enter(_,_,S,_),_).
stopId(S):-
        happensAt(my_stop_leave(_,_,S,_),_).

ttime(T):-
        happensAt(_,T).
ttime(T):-
        example(holdsAt(_,T)).

%*
not_happensAt(abrupt_acceleration_start(X,Y,Z),T):-
        not happensAt(abrupt_acceleration_start(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_abrupt_acceleration(Z),ttime(T).

not_happensAt(abrupt_acceleration_end(X,Y,Z),T):-
        not happensAt(abrupt_acceleration_end(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_abrupt_acceleration(Z),ttime(T).

not_happensAt(abrupt_deceleration_start(X,Y,Z),T):-
        not happensAt(abrupt_deceleration_start(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_abrupt_deceleration(Z),ttime(T).

not_happensAt(abrupt_deceleration_end(X,Y,Z),T):-
        not happensAt(abrupt_deceleration_end(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_abrupt_deceleration(Z),ttime(T).

not_happensAt(internal_temperature_change(X,Y,Z),T):-
        not happensAt(internal_temperature_change(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_internal_temperature(Z),ttime(T).

not_happensAt(noise_level_change(X,Y,Z),T):-
        not happensAt(noise_level_change(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_internal_temperature(Z),ttime(T).

not_happensAt(passenger_density_change(X,Y,Z),T):-
        not happensAt(passenger_density_change(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_passenger_density(Z),ttime(T).

not_happensAt(sharp_turn_start(X,Y,Z),T):-
        not happensAt(sharp_turn_start(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_passenger_density(Z),ttime(T).

not_happensAt(sharp_turn_end(X,Y,Z),T):-
        not happensAt(sharp_turn_end(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_sharp_turn(Z),ttime(T).

not_happensAt(my_stop_enter(X,Y,stopId,Z),T):-
        not happensAt(my_stop_enter(X,Y,stopId,Z),T),
        vehicleId(X),vehicleType(Y),fv_my_stop_enter(Z),ttime(T).

not_happensAt(my_stop_leave(X,Y,stopId,Z),T):-
        not happensAt(my_stop_leave(X,Y,stopId,Z),T),
        vehicleId(X),vehicleType(Y),fv_my_stop_leave(Z),ttime(T).



not_holdsAt(sharp_turn(X,Y,Z),T):-
        not holdsAt(sharp_turn(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_sharp_turn(Z),ttime(T).

not_holdsAt(abrupt_acceleration(X,Y,Z),T):-
        not holdsAt(abrupt_acceleration(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_abrupt_acceleration(Z),ttime(T).  

not_holdsAt(abrupt_deceleration(X,Y,Z),T):-
        not holdsAt(abrupt_deceleration(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_abrupt_deceleration(Z),ttime(T).

not_holdsAt(passenger_density(X,Y,Z),T):-
        not holdsAt(passenger_density(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_passenger_density(Z),ttime(T).

not_holdsAt(noise_level(X,Y,Z),T):-
        not holdsAt(noise_level(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_noise_level(Z),ttime(T).

not_holdsAt(internal_temperature(X,Y,Z),T):-
        not holdsAt(internal_temperature(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_internal_temperature(Z),ttime(T).  

not_holdsAt(driving_quality(X,Y,Z),T):-
        not holdsAt(driving_quality(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_driving_quality(Z),ttime(T).

not_holdsAt(driving_style(X,Y,Z),T):-
        not holdsAt(driving_style(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_driving_style(Z),ttime(T).

not_initiatedAt(driving_style(X,Y,Z),T):-
        not initiatedAt(driving_style(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_driving_style(Z),ttime(T).

not_terminatedAt(driving_style(X,Y,Z),T):-
        not terminatedAt(driving_style(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_driving_style(Z),ttime(T).

not_holdsAt(punctuality(X,Y,Z),T):-
        not holdsAt(punctuality(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_punctuality(Z),ttime(T).

not_initiatedAt(punctuality(X,Y,Z),T):-
        not initiatedAt(punctuality(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_punctuality(Z),ttime(T).

not_terminatedAt(punctuality(X,Y,Z),T):-
        not terminatedAt(punctuality(X,Y,Z),T),
        vehicleId(X),vehicleType(Y),fv_punctuality(Z),ttime(T).
*%
 
fv_passenger_satisfaction(reducing).

fv_driving_quality(low).

fv_driving_style(unsafe).

fv_passenger_density(high).
fv_passenger_density(low).

fv_noise_level(high).
fv_noise_level(low).

fv_sharp_turn(sharp).
fv_sharp_turn(very_sharp).

fv_punctuality(punctual).
fv_punctuality(non_punctual).

fv_abrupt_acceleration(abrupt).
fv_abrupt_acceleration(very_abrupt).
fv_abrupt_deceleration(abrupt).
fv_abrupt_deceleration(very_abrupt).

fv_internal_temperature(very_cold).
fv_internal_temperature(cold).
fv_internal_temperature(normal).
fv_internal_temperature(warm).
fv_internal_temperature(very_warm).

fv_my_stop_enter(late).
fv_my_stop_enter(early).
fv_my_stop_enter(scheduled).
fv_my_stop_leave(late).
fv_my_stop_leave(early).
fv_my_stop_leave(scheduled).


	


