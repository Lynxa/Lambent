% =========================================================================================================================
%	 						Simulation Submodule: Objects Management
% =========================================================================================================================



:- use_module(library(anti_unify)).

% Reserves the following predicates:

% Reserves the following fluents:

% These predicates are updated during protocol execution:
:- dynamic identifier/1.



% =====================================================================
% Objects Management Predicates
% =====================================================================


% Keeps a list of identifiers used for unique object names.
identifier(0).


% Determines whether a more general class exists.
% Fails if class name does not exist.
class(Class) :- general_class(Class, _).
general_class(Class, Properties) :-
	clause(classes(Classes)),
	member((Class, Properties), Classes).


% Determines whether an object exists, in some state.
% Fails if object name does not exist.
% Fails if state at Time does not exist.
object(X) :- state(CurrentState), objectin(X, CurrentState).
objectat(X, Time) :- find_state_at(Time, State), objectin(X, State).
objectin(X, State) :- member((X,_), State).


% Used to get or test object property values, in some state.
% Fails if object name or property name or value do not exist.
% Fails if state at Time does not exist.
% Variable grounding propagates between queries in list.
value(X, P, V) :- value(X, [(P,V)]).
valueat(X, P, V, Time) :- valueat(X, [(P,V)], Time).
valuein(X, P, V, State) :- valuein(X, [(P,V)], State).

value(X, PVs) :- state(CurrentState), valuein(X, PVs, CurrentState).
valueat(X, PVs, Time) :- find_state_at(Time, State), valuein(X, PVs, State).
valuein(X, PVs, State) :- member((X, Characteristics), State), enlist(PVs, LPVs), includes(Characteristics, LPVs).


% Creates new objects as instances of some class, in the current state.
% If (part of) the object name is ungrounded, a unique name is returned.
% Class should be generalized by existing classes on (un)grounded variables.
% If multiple classes match, an arbitrary one is chosen.
% Run-time error if:
% - object name already exists,
% - class name does not exist.
create(X, Class) :-
	ground_object(X),
	(\+ object(X) -> true ;
        	error("(create) An object of this name already exists.")
        ),
	(general_class(Class, Properties) -> true ;
		error("(create) An appropriate class does not exist.")
	),
	convert([instance_of|Properties], Characteristics),
	retract(state(CurrentState)),
	assert(state([(X, Characteristics)|CurrentState])),
	set(X, instance_of, Class), !.


% Destroys existing objects, in the current state.
% Run-time error if:
% - object name is ungrounded,
% - object name does not exists.
destroy(X) :-
	(\+ nonground(X) -> true ;
		error("(destroy) The object name is not grounded.")
	),
	(object(X) -> true ;
		error("(destroy) An object of this name does not exist.")
	),
	retract(state(CurrentState)),
	delete((X,_), CurrentState, NewCurrentState),
	assert(state(NewCurrentState)), !.


% Used to set existing object properties to new values, in the current state.
% Run-time error if:
% - object/property name is ungrounded,
% - object/property name does not exists.
set(X, P, V) :-
	(\+ nonground((X,P)) -> true ;
		error("(set) The object/property name is not grounded.")
	),
	(object(X) -> true ;
		error("(set) An object of this name does not exist.")
	),
	(value(X,P,_) -> true ;
		error("(set) A property of this name does not exist.")
	),
	retract(state(CurrentState)),
	delete((X,Properties), CurrentState, NewCurrentState),
	delete((P,_), Properties, NewProperties),
	assert(state([(X,[(P,V)|NewProperties])|NewCurrentState])), !.


% =====================================================================

% Grounds the free variables of an object name to be unique in the current state.
ground_object(X) :- \+ nonground(X), !.
ground_object(X) :-
	nonground(X, L),
	repeat,
		retract(identifier(L)),
		L1 is L+1,
		assert(identifier(L1)),
	\+ object(X),
	ground_object(X), !.

% =====================================================================

% Converts a list of properties to a list of (property, value) pairs.
convert([], []).
convert([H|B], [(H,undefined)|CB]) :- convert(B, CB).

% =====================================================================

% Handles the exceptions caused by improper use of provided predicates.
error(Message) :-
	show(["\n"]),
	show(["[Kernel] ", "Simulation error: ", Message, "\n"]),
	show(["[Kernel] ", "Abnormal termination...\n"]),
	exit_block(error).

% =====================================================================

