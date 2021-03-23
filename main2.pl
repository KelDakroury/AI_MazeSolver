/*
run this code by typing 
"dfs." in the query part in prolog online compiler to run the backtracking
"bfs_." in the query part in prolog online compiler to run the BFS

Author: Karim ElDakroury

Resources & References:
https://github.com/MontyWest/prolog/

http://www.cse.unsw.edu.au/~billw/Justsearch.html


*/
:- retractall(size(_,_)).
:- retractall(covid(_,_)).
:- retractall(doctor(_,_)).
:- retractall(mask(_,_)).
:- retractall(home(_,_)).
:- dynamic doctor/2.
:- dynamic home/2.
:- dynamic mask/2.
:- dynamic size/2.
:- dynamic covid/2.

%pritning function

print_map :-
	print_map([]).
print_map(Path) :-
	size(Vert, Hori), nl,
    Hori1 is Hori - 1,
    Vert1 is Vert - 1,
	print_header(0,Hori1), nl,
	print_map_horiz_edge(0,Hori1), nl,
	print_inner_map(0, Vert1, Hori1, Path), nl,
	print_map_horiz_edge(0,Hori1).

print_spacer_top :-
	write('     '). %%5

print_spacer_corner :-
	write('   '). %%3

print_spacer_row :-
	write(' '). %%1

print_header(From, To) :-
	print_spacer_top,
	print_column_header(From, To).

/* Prints column header until false, hence the need for the cut
 which prevents back tracking after header is printed*/
print_column_header(To, To) :-
	write(To).
print_column_header(From, To) :-
	From>=To.
print_column_header(From, To) :-
	write(From),
	print_heading_inner_spacer(From),
	Next is From+1,
	print_column_header(Next, To),
	!.

print_map_horiz_edge(From, To) :-
	print_spacer_corner,
	write('+'),
	print_map_horiz_border(From, To),
	write('+').

/* Prints top/bottom border until false, hence the need for the cut
 which prevents back tracking after border is printed*/
print_map_horiz_border(To, To) :-
	write('---').
print_map_horiz_border(From, To) :-
	From>=To.	
print_map_horiz_border(From, To) :-
	write('--'),
	Next is From+1,
	print_map_horiz_border(Next, To),
	!.

/* Prints the inside of the map (below top border, above bottom border)
%% Cut prevents back tracking when mze is printed*/
print_inner_map(Row, Row, Columns, Path) :-
	print_map_row(Row, 0, Columns, Path).
print_inner_map(Row, RowTo, _, _) :-
	Row>=RowTo.
print_inner_map(Row, RowTo, Columns, Path) :-
	print_map_row(Row, 0, Columns, Path),
	nl,
	RowNext is Row+1,
	print_inner_map(RowNext, RowTo, Columns, Path),
	!.

print_heading_inner_spacer(Row) :-
	Row > 9 ; write(' '),
	true.

print_map_row(Row, ColumnFrom, ColumnTo, Path) :-
	print_spacer_row,
	write(Row),
	print_heading_inner_spacer(Row),
	write('|'),
	print_map_row_inner(Row, ColumnFrom, ColumnTo, Path).

/* Prints a row of the inner map, stops when it reachs map size
%% Cute prevents backtracking after row is printed*/
print_map_row_inner(Row, Column, Column, Path):-
	write(' '),
	print_symbol(Row, Column, Path),
	write(' |').
print_map_row_inner(_, Column, ColumnTo, _) :-
	Column>=ColumnTo.
print_map_row_inner(Row, Column, ColumnTo, Path) :-
	write(' '),
	print_symbol(Row, Column, Path),
	NextColumn is Column+1,
	print_map_row_inner(Row, NextColumn, ColumnTo, Path),
	!.

%% Symbols for the entities
print_symbol(Row, Column, Path) :-
	member([Row,Column], Path), write('a'). % Actor's path with "a"
print_symbol(Row, Column, _) :-
	doctor(Row, Column), write('D').  %Doctor with "D"
print_symbol(Row, Column, _) :-
	mask(Row, Column), write('M'). %Mask with "M"
print_symbol(Row, Column, _) :-
	covid(Row, Column), write('C').  %Covid with "C"
print_symbol(Row, Column, _) :-
	home(Row, Column), write('H'). %Home with "H"
print_symbol(Row, Column, _) :-
	actor(Row, Column), write('A'). %Actor with "A"
print_symbol(_, _, _) :-
	write('.').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


actor(0,0).

set_size :-
    write('Rows: '), read(X), write('Columns: '), read(Y), asserta(size(X,Y)).

% Generating covid 1 and Covid 2 with their zones
placecovid1 :-
size(A,B), random(0,A,X), random(0,B,Y),
    ( not(actor(X,Y)), (X=\=1, Y=\=0), (X=\=1, Y=\=1), (X=\=0, Y=\=1) )-> (
        write('Covid1 coordinates: '),write('('), write(X), write(','), write(Y),write(')'), nl,
        asserta(covid(X,Y)),
    A1 is X - 1, B1 is Y - 1, asserta(covid(A1, B1)), A2 is X - 1, B2 is Y,  asserta(covid(A2, B2)),
    A3 is X - 1, B3 is Y+1,   asserta(covid(A3, B3)), A4 is X,     B4 is Y+1,  asserta(covid(A4, B4)),
    A5 is X+1,   B5 is Y+1,   asserta(covid(A5, B5)),  A6 is X+1,   B6 is Y,     asserta(covid(A6, B6)),
      A7 is X+1,   B7 is Y-1,   asserta(covid(A7, B7)), A8 is X,     B8 is Y-1, asserta(covid(A8, B8))
    ) ; placecovid1.

placecovid2 :-
    size(A,B), random(0,A,X), random(0,B,Y),
    ( \+ actor(X,Y), \+ covid(X,Y), (X=\=1, Y=\=0), (X=\=1, Y=\=1),(X=\=0, Y=\=1)) -> (
        write('Covid 2: '),write('('), write(X), write(','), write(Y),write(')'), nl,
        asserta(covid(X,Y)),
        A1 is X - 1, B1 is Y - 1, asserta(covid(A1, B1)), A2 is X - 1, B2 is Y,  asserta(covid(A2, B2)),
    A3 is X - 1, B3 is Y+1,   asserta(covid(A3, B3)), A4 is X,     B4 is Y+1,  asserta(covid(A4, B4)),
    A5 is X+1,   B5 is Y+1,   asserta(covid(A5, B5)),  A6 is X+1,   B6 is Y,     asserta(covid(A6, B6)),
      A7 is X+1,   B7 is Y-1,   asserta(covid(A7, B7)), A8 is X,     B8 is Y-1, asserta(covid(A8, B8))
    )
    ;
    placecovid2.

placecovid:- placecovid1, placecovid2.

%Generating home, doctor, and mask in random places
placehome :- size(A,B), random(0,A,X),random(0,B,Y),
    (\+ actor(X,Y), \+ covid(X,Y)) -> (asserta(home(X,Y)), 
	write('Home coordinates: '),write('('), write(X), write(','), write(Y),write(')'), nl ); placehome.


placesafe:- placedoctor, placemask.
placedoctor :-
    size(A,B), random(0,A,X), random(0,B,Y),
    (\+ actor(X,Y), \+ covid(X,Y), \+ home(X,Y)) -> 
	(write('Doctor coords: '),write('('), write(X), write(','), write(Y),write(')'), nl, asserta(doctor(X,Y)) )
    ;
    placedoctor.

placemask:-
    size(A,B), random(0,A,X), random(0,B,Y),
    (\+ actor(X,Y), \+ covid(X,Y), \+ home(X,Y), \+ doctor(X,Y)) -> 
	( write('Mask coordinates: '),write('('), write(X), write(','), write(Y), write(')'), nl,asserta(mask(X,Y))   )
    ;
    placemask.

%checking valid moves
available_move([X0,Y0], [X1,Y1]) :- 
	adj_tile([X0,Y0], [X1,Y1]), available_tile(X1,Y1).

inside_map(X1,Y1) :- size(A,B), X1>=0, X1<A, Y1>=0, Y1<B.

available_tile([X,Y]) :- 
available_tile(X,Y).
available_tile(X0,Y0) :- 
inside_map(X0,Y0), \+(covid(X0,Y0)).

%% True if [X0, Y0] is a available_move to [X1, Y1]
adj_tile([X0,Y0], [X1,Y1]) :- (X1 is X0-1), (Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y1]) :- (X1 is X0-1), (Y1 is Y0+1).
adj_tile([X0,Y0], [X1,Y1]) :- (X1 is X0+1), (Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y1]) :- (X1 is X0+1), (Y1 is Y0+1).
adj_tile([X0,Y0], [X0,Y1]) :- (Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y0]) :- (X1 is X0-1).
adj_tile([X0,Y0], [X0,Y1]) :- (Y1 is Y0+1).
adj_tile([X0,Y0], [X1,Y0]) :- (X1 is X0+1).



% Backtracking with DFS

%% Checks endpoints first, then gets list of paths, then choses shortest from that list.
%% Cut after getting path list prevents back tracking after displaying all shortest lists
solve(ST, ET, Path) :-
	available_tile(ST),
	available_tile(ET),
	size(N,M),
	get_paths_list(ST, ET, N*M, [], PathList),
	!,
	get_shortest_path(Path, PathList),
	print_map(Path).

%% Plucks a path from list, if its length is minimal then true
get_shortest_path(Path, PathList) :-
	get_shortest_path_length(PathList, Min),
	member(Path, PathList),
	length(Path, Min).

%% Gets the minimum of the lengths of paths in the list (True if Min is the minimum length)
get_shortest_path_length([H|T], Min) :-
	length(H, MinStart),
	get_shortest_path_length(T, MinStart, Min).

%% Recurses through the list, taking the minimum length that it finds as it goes
get_shortest_path_length([], Min, Min).
get_shortest_path_length([NewPath|T], LatestMin, Min) :-
	length(NewPath, N),
	NewMin is min(N, LatestMin), 
	get_shortest_path_length(T, NewMin, Min).

%% recursively finds paths that satisify, when all are found
%% this returns false, and will move to next rule.
get_paths_list(ST, ET, MaxLength, Cumu, PathList) :-
	path_solver_plus(ST, ET, MaxLength, [ET], Path),
	length(Path, N),
	N =< MaxLength, %% redundant check assuming it works in the path solver
	\+ memberchk(Path, Cumu),
	get_paths_list(ST, ET, N, [Path|Cumu], PathList).

%% Order means this is only called when above rule fails (all paths are found)
%% acts as boundary condition for recursion.
get_paths_list(_, _, _, PathList, PathList).

%% Solves for path backwards (as it's easy to build list that way)
%% When the current tile is the start tile then end.
path_solver_plus(ST, ST, _, Path, Path):-
	!.
%% Sink effect on the start tile, if we are currently one tile away then move straight there.
path_solver_plus(ST, CurrentT, MaxLength, Cumu, Path) :-
	available_move(CurrentT, ST),
	length(Cumu, N),
	N < MaxLength,
	Path = [ST|Cumu],
	!.
%% Finds an available move, if we haven't been there, add to the path and recurse
%% If the current path being built gets bigger than max allowed length then not allowed
path_solver_plus(ST, CurrentT, MaxLength, Cumu, Path) :-
	length(Cumu, N),
	N < MaxLength,
	available_move(CurrentT, D),
	\+ memberchk(D, Cumu),
	path_solver_plus(ST, D, MaxLength, [D|Cumu], Path).


%  BFS
bfs(Start, Home, Solution) :-
  breadthfirst([[Start]], Home, Solution),
   print_map(Solution).
   
% breadthfirst([Path1, Path2, ...], Solution).
%  Solution is an extension to a goal of
%  one of the paths.

breadthfirst([[Home|Path]|_], Home, [Home|Path]).
  % Always first check if goal reached

% If not, then extend this path by all
% possible edges, put these new paths on the
% end of the queue (Paths1) to check, and do
% breadthfirst on this new collection of
% paths, Paths1:
breadthfirst([Path|Paths], Home, Solution) :-
  extend(Path, NewPaths),
  append(Paths, NewPaths, Paths1),
  breadthfirst(Paths1, Home, Solution).

% extend([N|Rest], NewPaths).
% Extend the path [N|Rest] using all edges
% through N that are not already on the path.
% This produces a list NewPaths.
extend([Node|Path], NewPaths) :-
  findall([NewNode, Node|Path], (available_move(Node, NewNode),
   \+ member(NewNode,[Node|Path])), NewPaths),
  !.
extend(Path,[]). %findall failed: no edge


map:-
    set_size, placecovid, placehome, placesafe, print_map.


dfs:-
    set_size, placecovid, placehome, placesafe, home(A,B), solve([0,0], [A,B], Path).

bfs_:-
    set_size, placecovid, placehome, placesafe, home(A,B), bfs([0,0], [A,B], Path).
