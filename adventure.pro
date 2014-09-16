%% Nani Search - Adventure

%% Game initialization
:- initialization(init_game).

%% Here I would load the other prolog files, I guess
init_game :-
    load_test_files([]).


%% Rooms
room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).


%% Contents
:- dynamic contains/2.
contains(office, desk).
contains(office, computer).
contains(desk, flashlight).
contains(desk, box).
contains(box, ticket).
contains(kitchen, apple).
contains(kitchen, broccoli).
contains(kitchen, crackers).
contains(kitchen, table).
contains(cellar, 'washing machine').
contains('washing machine', nani).


%% an object is at a location if it is directly there
%% or in one of the objects contained in it, recursively
location(Object, Place) :-
    contains(Place, Object).
location(Object, Outer) :-
    contains(Outer, Place),
    location(Object, Place).


%% Directions and their reverses
direction_pair(n, s).
direction_pair(e, w).
direction_pair(up, down).
direction_pair(in, out).

direction(X) :- direction_pair(X, _).
direction(X) :- direction_pair(_, X).


%% New exit system
:- dynamic link/2.
:- dynamic one_way_link/2.
link(hall, office).
link(kitchen, office).
link(hall, 'dining room').
link(kitchen, cellar).
link('dining room', kitchen).
one_way_link(hall, library).

% there is a regular link
linked(X, Y) :- link(X, Y), !.
% or we can deduce that there is a link because the reverse exists
linked(X, Y) :- link(Y, X), !.
% or it could also be a one way link
linked(X, Y) :- one_way_link(X, Y), !.


%% Doors
:- dynamic door/3.
door(office, hall, closed).

is_closed(X,Y) :- door(X,Y,closed).
is_closed(X,Y) :- door(Y,X,closed).

% If a door cannot be proven to be closed, then it's open
% Including the case where the door doesn't exist at all :)
is_open(X, Y) :- \+ is_closed(X, Y).


%% Food
tastes_yucky(broccoli).

edible(apple).
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
    linked(Place, X),
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
goto(Place) :-
    can_go(Place),
    move(Place),
    look,
    !.

can_go(Place) :-
    here(X),
    X \= Place,
    linked(X, Place),
    is_open(X, Place).

can_go(Place) :-
    here(Place),
    format("You are already in the ~s.", Place), nl,
    !, fail.

can_go(Place) :-
    here(X),
    is_closed(X, Place),
    write("The door is closed."), nl,
    !, fail.

can_go(_) :-
    write("You can't get there from here."), nl,
    fail.

move(Place) :-
    retract(here(_)),
    asserta(here(Place)).


%% Inventory
take(X) :-
    can_take(X),
    take_object(X),
    !.

can_take(Thing) :-
    here(Place),
    location(Thing, Place).
can_take(Thing) :-
    format('There is no ~s here.', Thing), nl,
    fail.

take_object(Object) :-
    contents(Place, X),
    member(Object, X),
    subtract(X, [Object], Result),
    retract(contents(Place, _)),
    asserta(contents(Place, Result)),
    asserta(have(Object)),
    write("Taken."), nl.

put(Object, Container) :-
    have(Object),
    location(Container, Location),
    here(Location),
    put_object(Object, Container),
    !.

put_object(Object, Container) :-
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
    asserta(door(X, L, open)),
    !.

open_door(X) :-
    here(L),
    door(L, X, closed),
    retract(door(L, X, closed)),
    asserta(door(L, X, open)),
    !.

close_door(X) :-
    here(L),
    door(X, L, open),
    retract(door(X, L, open)),
    asserta(door(X, L, closed)),
    !.

close_door(X) :-
    here(L),
    door(L, X, open),
    retract(door(L, X, open)),
    asserta(door(L, X, closed)),
    !.
