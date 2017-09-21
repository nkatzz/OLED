%%%%%% stopped %%%%%%

initiatedAt(stopped(Vessel) = true, T) :-
    happensAt(stop_start(Vessel),T),
    happensAt(coord(Vessel, Lon, Lat), T),
    nearPorts(Lon,Lat,[]),
    not happensAt(stop_end(Vessel), T).

terminatedAt(stopped(Vessel) = true, T) :-
    happensAt(stop_end(Vessel), T).

%%%%%% lowspeed %%%%%%

initiatedAt(lowSpeed(Vessel) = true, T) :-
    happensAt(slow_motion_start(Vessel), T),
    happensAt(coord(Vessel, Lon, Lat), T),
    nearPorts(Lon,Lat,[]),
    not happensAt(slow_motion_end(Vessel), T),
    not happensAt(start(stopped(Vessel) = true), T).

terminatedAt(lowSpeed(Vessel) = true, T) :-
    happensAt(slow_motion_end(Vessel), T).

terminatedAt(lowSpeed(Vessel) = true, T) :-
    happensAt(start(stopped(Vessel) = true), T).

%%%%%% withinArea %%%%%%

initiatedAt(withinArea(Vessel, AreaName) = true, T) :-
    happensAt(isInArea(Vessel, AreaName), T).

terminatedAt(withinArea(Vessel, AreaName) = true, T) :-
    happensAt(leavesArea(Vessel, AreaName), T).

%%%%%% sailing %%%%%%

initiatedAt(sailing(Vessel) = true, T) :-
    happensAt(velocity(Vessel, Speed, Heading), T),
    Speed > 2.0,
    not happensAt(gap_start(Vessel), T).

terminatedAt(sailing(Vessel) = true, T) :-
    happensAt(velocity(Vessel, Speed, Heading), T),
    Speed < 2.0.

terminatedAt(sailing(Vessel) = true, T) :-
    happensAt(gap_start(Vessel), T).

%%%%%% high-speed-in %%%%%%

initiatedAt(highSpeedIn(Vessel, AreaName) = true, T) :-
    happensAt(isInArea(Vessel, AreaName), T),
    happensAt(velocity(Vessel, Speed, Heading), T),
    speedArea(AreaName,SpeedArea),
    Speed > SpeedArea,
    not happensAt(gap_start(Vessel), T).

terminatedAt(highSpeedIn(Vessel, AreaName) = true, T) :-
    happensAt(isInArea(Vessel, AreaName), T),
    happensAt(velocity(Vessel, Speed, Heading), T),
    speedArea(AreaName,SpeedArea),
    Speed < SpeedArea.

terminatedAt(highSpeedIn(Vessel, AreaName) = true, T) :-
    happensAt(leavesArea(Vessel, AreaName), T).

terminatedAt(highSpeedIn(Vessel, AreaName) = true, T) :-
    happensAt(gap_start(Vessel), T).

%%%%%% loitering %%%%%%

holdsFor(loitering(Vessel) = true, I) :-
    holdsFor(lowSpeed(Vessel) = true, Il),
    holdsFor(stopped(Vessel) = true, Is),
    holdsFor(withinArea(Vessel, AreaName) = true, Ia),
    intersect_all([Il, Ia], Ila),
    intersect_all([Is, Ia], Isa),
    union_all([Ila, Isa], If),
    findall((S,E), (member((S,E), If), Diff is E - S, Diff > 600), I).

%%%%%% rendezVouz %%%%%%

holdsFor(rendezVouz(Vessel1, Vessel2) = true, I) :-
    holdsFor(proximity(Vessel1, Vessel2) = true, Ip),
    holdsFor(lowSpeed(Vessel1) = true, Il1),
    holdsFor(lowSpeed(Vessel2) = true, Il2),
    holdsFor(stopped(Vessel1) = true, Is1),
    holdsFor(stopped(Vessel2) = true, Is2),
    union_all([Il1, Is1], I1),
    union_all([Il2, Is2], I2),
    intersect_all([I1, I2, Ip], If),
    findall((S,E), (member((S,E), If), Diff is E - S, Diff > 600), I).
