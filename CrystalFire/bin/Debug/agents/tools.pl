% =====================================================================
% Tools Library
% =====================================================================

:- ["../utils/utils"].

% =====================================================================

% Pretty prints a state.
view_state(State) :-
	forall((member((X,Characteristics), State),
		show(agent_state_objects, ["\n  Object Name: ", X, "\n"]),
		forall((member((P,V), Characteristics),
			show(agent_state_properties, ["    ", P, " = ", V, "\n"])
		))
	)).

% =====================================================================

% Used by agents to process their local copies of the simulation state.
objectin(X, State) :- member((X,_), State).
valuein(X, P, V, State) :- valuein(X, [(P,V)], State).
valuein(X, PVs, State) :- member((X, Characteristics), State), enlist(PVs, LPVs), includes(Characteristics, LPVs).

% =====================================================================

% Use by agents to reconstruct the state, given its difference from the previous one.
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

