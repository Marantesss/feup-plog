% RED BULL AIR RACE

% drivers
pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

% teams (not necessary)
team(breitling).
team(red_bull).
team(mediterranean_racing_team).
team(cobra).
team(matador).

% pilot's teams
pilot_team(lamb, breitling).

pilot_team(besenyei, red_bull).
pilot_team(chambliss, red_bull).

pilot_team(maclean, mediterranean_racing_team).

pilot_team(mangold, cobra).

pilot_team(jones, matador).
pilot_team(bonhomme, matador).

% pilot's planes
pilot_plane(lamb, mx2).

pilot_plane(besenyei, edge540).
pilot_plane(chambliss, edge540).
pilot_plane(maclean, edge540).
pilot_plane(mangold, edge540).
pilot_plane(jones, edge540).
pilot_plane(bonhomme, edge540).

% tracks
track(instanbul).
track(budapest).
track(porto).

% victories
victory_track(jones, porto).
victory_track(mangold, budapest).
victory_track(mangold, instanbul).

% gates per track
track_gates(instanbul, 9).
track_gates(budapest, 6).
track_gates(porto, 5).

% team won track
team_victory(Track, Team):-
    victory_track(Pilot, Track),
    pilot_team(Pilot, Team).

won_at_least_2(Pilot):-
    victory_track(Pilot, Track_1),
    victory_track(Pilot, Track_2),
    Track_1 \= Track_2.

track_more_than_8(Track):-
    track_gates(Track, Gates),
    Gates > 8.

pilot_not_edge(Pilot):-
    pilot_plane(Pilot, Plane),
    Plane \= edge540.

% a) victory_track(Pilot, porto).
% b) team_victory(porto, Team).
% c) won_at_least_2(Pilot).
% d) track_more_than_8(Track).
% e) pilot_not_edge(Pilot).
