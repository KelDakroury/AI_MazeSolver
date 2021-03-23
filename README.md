# AI_MazeSolver
Developed a Smart Maze Solver using Prolog language


## Preface and PEAS:

During pandemic times, we need to keep ourselves and others safe. The actor’s goal is to reach home safely by avoiding crowded places which would make him catch Covid-19 if he passed through them without the mask. Thus, the actor needs to visit a doctor to have vaccinate from Covid-19, or to buy a mask if he will pass near a crowded point. The Performance is measured by the time the actor will spend to reach the home. The environment here is partially observabl environment, as n a partially observable system the observer may utilize a memory system to add information to the observer's understanding of the system. The actuator is the actor, the map, covid, doctor, the mask, and the home place. The sensing part here counts on saving the visited tiles and knowing the places of covid, home, mask, and the doctor.

## Task Description:

### The map:

The as no specific size, it takes the user input for a desired map size, but there re some restrictions for
the map size as it should not be less than 5*5. This is because the code books at least 18 tiles for covid, 1
tile for the home, 1 tile for the actor, 1 tile for the doctor, and 1 tile for the mask.

The entities of the map are denoted by the following symbols:

- The Actor and his path with “A”
- The Covid tiles and their surrounding zone with “C”
- The Doctor with “D”
- The Mask with “M”
- The home with “H” – it changes to “A” when the actor reaches it-

### The Actor

The actor’s main goal is to reach the home, and it has a (0, 0) coordinates as a starting position always.
`<actor(0,0).>`

### Covid-

The map contains 2 Covid tiles, each of them is surrounded by 8 tiles which are infected with Covid too.
The difference between the Covid tile itself and the surrounding one is that the actor can go through the
surrounding zone of the Covid using only a mask “Passed through the mask’s tile”, but it cannot enter
the Covid tile except if he has vaccine from the Doctor “passed through the doctor’s tile”. The covid cells
are randomly generated in the map. The following figure shows the random generating implementation:

```
<placecovid1 :-
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

placecovid:- placecovid1, placecovid2.>
```

### The Doctor

The doctor’s roll is to make the actor able to pass through the Covid tiles, and their surrounding zone too. The map consists of one doctor which is randomly generated. The following figure shows its
implementation in the code:

```
<placedoctor :-
    size(A,B), random(0,A,X), random(0,B,Y),
    (\+ actor(X,Y), \+ covid(X,Y), \+ home(X,Y)) -> 
	(write('Doctor coords: '),write('('), write(X), write(','), write(Y),write(')'), nl, asserta(doctor(X,Y)) )
    ;
    placedoctor.>
```
### The Mask

The mask makes the actor able to pass through the surrounding zone of Covid tiles. The map consists of
one Mask which is randomly generated. The following figure shows its implementation in the code:
```
<placemask:-
    size(A,B), random(0,A,X), random(0,B,Y),
    (\+ actor(X,Y), \+ covid(X,Y), \+ home(X,Y), \+ doctor(X,Y)) -> 
	( write('Mask coordinates: '),write('('), write(X), write(','), write(Y), write(')'), nl,asserta(mask(X,Y))   )
    ;
    placemask.>
```
## Solving techniques:

The actor reaches his home by using two ways. First of them is the Backtracking technique. And the
second one is Breadth-First Search algorithm

### Backtracking technique

Backtracking can be defined as a general algorithmic technique that considers searching every possible
combination to solve a computational problem [1]. The Backtracking is done using Dept-First Search
algorithm which is making a tree with all the
possible moves. But the algorithm is modified as
it only searches for a path using the valid tiles
“tiles which the actor can pass through to reach
home”.

The program is using Dynamic Programming and
iterative deepening technique. The following
figure shows a part of the backtracking
implementation:


### Breadth-First Search algorithm

The Breadth First Search algorithm is a common way to solve node-based path executions. Given a
graph of nodes, BFS will basically collect all the
possible paths that can be traveled and visit them
until the destination node is reached [1]. The BFS
consists of three main arrays: node_queue,
visited_nodes, and traveled_path. They are storing
data about the progress of the BFS.

- node_queue is storing nodes to travel.
- visited_nodes is storing nodes already
    traveled.
- traveled_path is storing nodes that
    traveled, have led to the destination.
    
