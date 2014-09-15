%% Nani Search - Adventure

%% Deleting thingies in case of repeated loading
%% Not sure of how idiomatic is this...
%% Or how reliably it works... (5.x vs 7.x)

%% Rooms
room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).


%% Contents
:- dynamic contents/2.
contents(office, [desk, computer]).
contents(desk, [flashlight]).
contents(kitchen, [apple, broccoli, crackers, table]).
contents(cellar, ['washing machine']).
contents('washing machine', [nani]).

%% make the crackers a container (?)
contents(table, []).


%% an object is at a location if it is amongst its contents
%% TODO: nested objects
location(Object, Place) :-
    contents(Place, X),
    member(Object, X).

%% Directions and their reverses
direction_pair(n, s).
direction_pair(e, w).
direction_pair(up, down).
direction_pair(in, out).

direction(X) :- direction_pair(X, _).
direction(X) :- direction_pair(_, X).

%% Doors / Exits
:- dynamic door/3.
door(office, hall, closed).
door(kitchen, office, closed).
door(hall, 'dining room', closed).
door(kitchen, cellar, closed).
door('dining room', kitchen, closed).

connect(X,Y) :- door(X,Y,_).
connect(X,Y) :- door(Y,X,_).

is_open(X,Y) :- door(X,Y,open).
is_open(X,Y) :- door(Y,X,open).

is_closed(X, Y) :- \+ is_open(X, Y).

%% Food
tastes_yucky(broccoli).

Edible(apple).
edible(crackers).

edible(X) :- tastes_yucky(X).

eat(X) :-
    have(X),
    edible(X),
    eat_food(X),
    !.

eat(X) :-
    have(X),
    format("You can't eat ~s...'", X), nl,
    fail.

eat(X) :-
    format("You don't have ~s.", X), nl,
    fail.

eat_food(X) :-
    tastes_yucky(X),
    retract(have(X)),
    ansi_format([fg(red)], "Tastes yucky...", []), nl.

eat_food(X) :-
    not(tastes_yucky(X)),
    retract(have(X)),
    ansi_format([fg(green)], "Refreshing!", []), nl.


%% Lights    
:- dynamic turned_off/1.
:- dynamic turned_on/1.
turned_off(flashlight).

turn_on(X) :-
    turned_off(X),
    retract(turned_off(X)),
    asserta(turned_on(X)).

turn_off(X) :-
    turned_on(X),
    retract(turned_on(X)),
    asserta(turned_off(X)).


%% Player stuff
:- dynamic here/1.
here(kitchen).

:- dynamic have/1.


%% Listing
list_things(Place) :-  
    location(X, Place),
    tab(2),
    write(X),
    nl,
    fail.
list_things(_).

list_connections(Place) :-
    connect(Place, X),
    tab(2),
    write(X),
    nl,
    fail.
list_connections(_).


%% Commands and auxiliary stuff
%% Some stuff can surely be better organised
look :-
    here(Place),
    ansi_format([bold,fg(red)], "~s", [Place]), nl,
    write('You can see:'), nl,
    list_things(Place),
    write('You can go to:'), nl,
    list_connections(Place),
    !.

look_in(Container) :-
    here(Place),
    location(Container, Place),
    format('You look into the ~s.', Container), nl,
    write('You can see:'), nl,
    list_things(Container),
    !.


%% Movement
goto(Place):-  
    can_go(Place),
    move(Place),
    look,
    !.

can_go(Place):-
    here(X),
    connect(X, Place),
    is_open(X, Place).

can_go(Place):-
    here(X),
    connect(X, Place),
    write("The door is closed."), nl,
    fail.

can_go(_):-
    write("You can't get there from here."), nl,
    fail.

move(Place):-
    retract(here(_)),
    asserta(here(Place)).


%% Inventory
take(X):-  
    can_take(X),
    take_object(X),
    !.

can_take(Thing) :-
    here(Place),
    location(Thing, Place).
can_take(Thing) :-
    format('There is no ~s here.', Thing), nl,
    fail.

take_object(Object):-
    contents(Place, X),
    member(Object, X),
    subtract(X, [Object], Result),
    retract(contents(Place, _)),
    asserta(contents(Place, Result)),
    asserta(have(Object)),
    write("Taken."), nl.

put(X, Y):-  
    have(X),
    location(Y, L),
    here(L),
    put_object(X, Y),
    !.

put_object(Object, Container):-  
    have(Object),
    contents(Container, List),
    append(List, Object, NewList),
    retract(contents(Container, _)),
    asserta(contents(Container, NewList)),
    retract(have(Object)),
    format('You put ~s into ~s.', [Object, Container]), nl.

inventory :-
    write('You are carrying:'), nl,
    list_inventory.

list_inventory :-  
    have(X),
    tab(2),
    write(X),
    nl,
    fail.


%% Door management
open_door(X) :-
    here(L),
    door(X, L, closed),
    retract(door(X, L, closed)),
    asserta(door(X, L, open)).

open_door(X) :-
    here(L),
    door(L, X, closed),
    retract(door(L, X, closed)),
    asserta(door(L, X, open)).

close_door(X) :-
    here(L),
    door(X, L, open),
    retract(door(X, L, open)),
    asserta(door(X, L, closed)).

close_door(X) :-
    here(L),
    door(L, X, open),
    retract(door(L, X, open)),
    asserta(door(L, X, closed)).


