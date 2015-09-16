% =========================================================================================================================
%	 						Simulation Submodule: State Management
% =========================================================================================================================

% A state is just a set of objects with specified values for their properties.

% Only the current state is kept in memory. Every past state is written in a DB.


% Reserves the following predicates:

% Reserves the following fluents:

% These predicates are updated during protocol execution:
:- dynamic state/1.



% =====================================================================
% State Management Predicates
% =====================================================================


% Keeps a list of all the objects and the values of their properties.
state([]).

% =====================================================================

% Create a new state and project everything from preceeding state.
create_first_state :-
	db_clear,
	time(Time),
	create(clock, event),
	set(clock, description, state_instantiation),
	set(clock, happened_at, Time),
	set(clock, expired_at, next_state_instantiation), !.

create_new_state :-
	value(clock, instance_of, event),
	value(clock, description, state_instantiation),
	time(Time),
	set(clock, expired_at, Time),
	retract(state(CurrentState)),
	db_open, db_store(state(CurrentState)), db_close,
	copy_term(CurrentState, NewState),
	assert(state(NewState)),
	clear_expired_events(Time),
	create(clock, event),
	set(clock, description, state_instantiation),
	set(clock, happened_at, Time),
	set(clock, expired_at, next_state_instantiation), !.

% =====================================================================

clear_expired_events(Time) :-
	forall((
		object(X),
		value(X, instance_of, event),
		value(X, expired_at, ETime),
		ETime @=< Time,
		destroy(X)
	)).

% =====================================================================

find_state_at(Time, State) :-
	db_open,
	(clause(state(State)) ; db_retrieve(state(State))),
	valuein(clock, instance_of, event, State),
	valuein(clock, description, state_instantiation, State),
	valuein(clock, happened_at, STime, State),
	valuein(clock, expired_at, ETime, State),
	STime @=< Time,
	(Time @< ETime ; ETime == next_state_instantiation), 
	db_close, !.

% =====================================================================

backup_state(BackupState) :-
	state(CurrentState),
	copy_term(CurrentState, BackupState).

restore_state(BackupState) :-
	retract(state(_CurrentState)),
	assert(state(BackupState)).

% =====================================================================

% Pretty prints a state.
view_state(State) :-
	forall((member((X,Characteristics), State),
		show(complete_state_objects, ["\n  Object Name: ", X, "\n"]),
		forall((member((P,V), Characteristics),
			show(complete_state_properties, ["    ", P, " = ", V, "\n"])
		))
	)).

% =====================================================================

state_difference(OldState, NewState, DeletedObjects, DeletedProperties, UpdatedProperties) :-
	findall(X, (
		objectin(X, OldState),
		\+ objectin(X, NewState)
	), DeletedObjects),

	findall((X,Ps), (
               	objectin(X, OldState),
		objectin(X, NewState),
		findall(P, (
			valuein(X, P, _, OldState),
			\+ valuein(X, P, _, NewState)
		), Ps),
		Ps \= []
	), DeletedProperties),

	findall((X, UpdatedCharacteristics), (
		member((X, Characteristics), NewState),
		(objectin(X, OldState) -> (
			member((X, OldCharacteristics), OldState),
                        subtract(Characteristics, OldCharacteristics, UpdatedCharacteristics),
			UpdatedCharacteristics \= []
		) ; (
			UpdatedCharacteristics = Characteristics
                ))
	), UpdatedProperties). % Includes new objects, new properties, and new values.


state_reconstruct(OldState, DeletedObjects, DeletedProperties, UpdatedProperties, NewState) :-
	findall((X, PersistentCharacteristics), (
		member((X, Characteristics), OldState),
		\+ member(X, DeletedObjects),
		(member((X,Ps), DeletedProperties) -> (
			findall((P,V), (
				member((P,V), Characteristics),
				\+ member(P, Ps)
			), PersistentCharacteristics)
		) ; true)
	), PersistentState),

	findall((X, NewCharacteristics), (
		(
			member((X, NewCharacteristics), UpdatedProperties),
			\+ member((X, _Characteristics), PersistentState)
		) ; (
			member((X, Characteristics), PersistentState),
			(member((X, UpdatedCharacteristics), UpdatedProperties) -> (
				findall((P,V), (
					once((member((P,V), UpdatedCharacteristics) ;
                	        	member((P,V), Characteristics)))
				), NewCharacteristics)
			) ; NewCharacteristics = Characteristics)
		)
	), NewState).

% =====================================================================

