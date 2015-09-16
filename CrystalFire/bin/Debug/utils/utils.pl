% =====================================================================
% # Utilities Library
% =====================================================================

:- get_flag(library_path, LP), set_flag(library_path, ["."|LP]).
:- use_module(library(sockets)).
:- use_module(library(random)).
:- randomise.
:- local flatten/2.

:- dynamic show_detail/1.

% =====================================================================

show([]) :- flush(stdout).
show([H|B]):- write(H), show(B).

show(MessageType, List) :-
	((	clause(show_detail(AllowedMessageTypes)),
		member(MessageType, AllowedMessageTypes)
	) -> show(List) ; true ).

% =====================================================================

say(X) :- show(["DEBUG: ", X, "\n"]).

% =====================================================================

succeed(Goal) :-
	once((call(Goal) ; true)).

% =====================================================================

receive(Stream, Delay, Message) :-
	select([Stream], Delay, [Stream]),
	read(Stream, Message).

receive(Stream, Message) :-
	receive(Stream, 0, Message).

% =====================================================================

send(Stream, Message) :-
	writeq(Stream, Message),
	write(Stream, '. '),
	flush(Stream).

% =====================================================================

% If probabilities sum up to less than 1, the predicate fails with the excess probability.
% If probabilities sum up to more than 1, the distribution above 1 is ignored.

choose(Choice, Distribution) :-
	frandom(R),
	choose_aux(Choice, R, Distribution).

choose_aux(Choice, R, [(P, C)|RestDistribution]) :-
	((R < P) -> (
		Choice = C
	) ; (
		RestR is R - P,
		choose_aux(Choice, RestR, RestDistribution)
	)).

% =====================================================================

% Goal is executed until it fails.
% Predicate always succeeds.
forall(Goal) :-
	findall(_, Goal, _).

% =====================================================================

% Goal is executed once per Condition.
% Predicate fails if for some Condition the Goal does not succeed.
foreach(Condition, Goal) :-
	\+ (call(Condition), call(\+ Goal)).

% =====================================================================

% resatisfiable()

% =====================================================================

enlist([], []) :- !.
enlist([H|B], [H|B]) :- !.
enlist(E, [E]) :- !.

% =====================================================================

flatten([], []).
flatten([H|B], List) :-
	flatten(B, List1),
	append(H, List1, List).

% =====================================================================

partition([], []).
partition([H], [[H]]).
partition([H,S|B], Partition) :-
	partition([S|B], [First|Rest]),
	(Partition = [[H],First|Rest] ;
	Partition = [[H|First]|Rest]).

% =====================================================================

includes([], []).
includes([_|_], []).
includes(List, [H|B]) :-
	member(H, List),
	includes(List, B).

% =====================================================================

bodytolist(L, [L]) :-
	(_,_) \= L.
bodytolist((H,B), [H|BL]) :-
	bodytolist(B, BL).

% =====================================================================

listtoterm([], true).
listtoterm([H|B], (H,T)):-
	listtoterm(B,T).

% =====================================================================


time(Time) :-
	statistics(times, [_,_,Time]).
%	date(Date), split_string(Date, " \n", "\n", [_WeekDay, _Month, _Day, Time, _Year]).

% =====================================================================

