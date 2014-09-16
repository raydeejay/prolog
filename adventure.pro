%% Nani Search - Adventure

%% Game initialization
main :- init_game.

%% Here I would load the other prolog files, I guess
init_game :-
    load_test_files([]),
    init_dynamic_facts,     % predicates which are not compiled

    write('NANI SEARCH - A Sample Adventure Game'), nl,
    write('Copyright (C) Amzi! inc. 1990-2010'), nl,
    write('No rights reserved, use it as you wish'), nl,
    nl,
    write('Hit any key to continue.'), get0(_),
    write('Type "help" if you need more help on mechanics.'), nl,
    write('Type "hint" if you want a big hint.'), nl,
    write('Type "quit" if you give up.'), nl,
    nl,
    write('Enjoy the hunt.'), nl,
    
    look,                   % give a look before starting the game
    command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop :-
    repeat,
    get_command(X),
    do(X),
    (nanifound; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     listener are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

do(goto(X)) :- goto(X), !.
do(nshelp) :- nshelp, !.
do(hint) :- hint, !.
do(inventory) :- inventory, !.
do(take(X)) :- take(X), !.
do(drop(X)) :- drop(X), !.
do(eat(X)) :- eat(X), !.
do(look) :- look, !.
do(turn_on(X)) :- turn_on(X), !.
do(turn_off(X)) :- turn_off(X), !.
do(look_in(X)) :- look_in(X), !.
do(quit) : -quit, !.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "have(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

nanifound :-
    have(nani),        
    write('Congratulations, you saved the Nani.'), nl,
    write('Now you can rest secure.'), nl, nl.

quit :-
    write('Giving up?  It''s going to be a scary night'), nl,
    write('and when you get the Nani it''s not going'), nl,
    write('to smell right.'), nl, nl.

% The help command

nshelp :-
    write('Use simple English sentences to enter commands.'), nl,
    write('The commands can cause you to:'), nl,
    nl,
    write('   go to a room          (ex. go to the office)'), nl,
    write('   look around           (ex. look)'), nl,
    write('   look in something     (ex. look in the desk)'), nl,
    write('   take something        (ex. take the apple)'), nl,
    write('   drop something        (ex. drop the apple)'), nl,
    write('   eat something         (ex. eat the apple)'), nl,
    write('   turn something on     (ex. turn on the light)'), nl,
    write('   inventory your things (ex. inventory)'), nl,
    nl,
    write('The examples are verbose, terser commands and synonyms'), nl,
    write('are usually accepted.'), nl, nl,
    write('Hit any key to continue.'), nl,
    get0(_),
    look.

hint :-
    write('You need to get to the cellar, and you can''t unless'), nl,
    write('you get some light.  You can''t turn on the cellar'), nl,
    write('light, but there is a flashlight in the desk in the'), nl,
    write('office you might use.'), nl, nl,
    look.


% respond simplifies writing a mixture of literals and variables
 
respond([]) :-
  write('.'), nl, nl.
respond([H|T]) :-
  write(H),
  respond(T).


% Simple English command listener.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C) :-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X, L, []),    % call the grammar for command
  C =.. X, !.          % make the command list a structure
get_command(_) :-
  respond(['I don''t understand, try again or type help']),fail.

% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a 
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred, Arg]) --> verb(Type, Pred), nounphrase(Type, Arg).
command([Pred]) --> verb(intran, Pred).
command([goto, Arg]) --> noun(go_place, Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place, goto) --> go_verb.
verb(thing, V) --> tran_verb(V).
verb(intran, V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go, to].
go_verb --> [g].

tran_verb(take) --> [take].
tran_verb(take) --> [pick, up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put, down].
tran_verb(eat) --> [eat].
tran_verb(turn_on) --> [turn, on].
tran_verb(turn_on) --> [switch, on].
tran_verb(turn_off) --> [turn, off].
tran_verb(look_in) --> [look, in].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].

intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look, around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(quit) --> [end].
intran_verb(quit) --> [bye].
intran_verb(nshelp) --> [help].
intran_verb(hint) --> [hint].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det, noun(Type, Noun).
nounphrase(Type,Noun) --> noun(Type, Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, or things located somewhere.  We define
% special cases for those things represented in Nani Search by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'dining room') --> [dining, room].

noun(thing, T) --> [T], {location(T, _)}.
noun(thing, T) --> [T], {have(T)}.
noun(thing, flashlight) --> [flash, light].
noun(thing, 'washing machine') --> [washing, machine].
noun(thing, 'dirty clothes') --> [dirty, clothes].

% If the player has just typed light, it can be interpreted three ways.
% If a room name is before it, it must be a room light.  If the
% player has the flash light, assume it means the flash light.  Otherwise
% assume it is the room light.

noun(thing, light) --> [X, light], {room(X)}.
noun(thing, flashlight) --> [light], {have(flashlight)}.
noun(thing, light) --> [light].

% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L) :-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C, W1, C1),        % Else read next word and rest of sentence
  restsent(C1, Ws).

readword(C, W, C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !, 
  name(W, [C]),             % get as an atom
  get0(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C, W, C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until 
  restword(C1, Cs, C2),       % we have all the word     
  name(W, [NewC|Cs]).       % then make it an atom
readword(C, W, C2) :-         % otherwise
  get0(C1),       
  readword(C1, W, C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'', 0'').
in_word(0'-, 0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :- 
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 0'9,
  C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).


%% Rooms
room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).


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


% These facts are all subject to change during the game, so rather
% than being compiled, they are "asserted" to the listener at
% run time.  This predicate is called when "nanisrch" starts up.

:- dynamic contains/2.
:- dynamic turned_off/1.
:- dynamic turned_on/1.

init_dynamic_facts :-
    assertz(contains(office, desk)),
    assertz(contains(office, computer)),
    assertz(contains(desk, flashlight)),
    assertz(contains(desk, box)),
    assertz(contains(box, ticket)),
    assertz(contains(kitchen, apple)),
    assertz(contains(kitchen, broccoli)),
    assertz(contains(kitchen, crackers)),
    assertz(contains(kitchen, table)),
    assertz(contains(cellar, 'washing machine')),
    assertz(contains('washing machine', nani)),
    assertz(turned_off(flashlight)).


%% Furniture
furniture(desk).
furniture('washing machine').
furniture(table).


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
take(Thing) :-
    format('There is no ~s here.', Thing), nl,
    fail.

can_take(Thing) :-
    here(Place),
    location(Thing, Place).

take_object(Object) :-
    retract(contains(_, Object)),
    asserta(have(Object)),
    write("Taken."), nl.

put(Object, Container) :-
    have(Object),
    location(Container, Location),
    here(Location),
    put_object(Object, Container),
    !.

put_object(Object, Container) :-
    retract(have(Object)),
    asserta(contains(Container, Object)),
    format('You put ~s into ~s.', [Object, Container]), nl.

% Inventory

inventory :-
    have(_),
    write('You are carrying:'), nl,
    list_inventory,
    !.
inventory :-
    write("You have nothing."), nl.

list_inventory :-
    have(X),
    tab(2),
    write(X),
    nl,
    fail.
list_inventory.


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
