% =========================================================================================================================
%	 						Simulation Submodule: Allowed Management
% =========================================================================================================================



% Reserves the following predicates:
% allowed/2.



% =====================================================================
% Allowed Management Predicates
% =====================================================================

% Used to determine whether an agent can execute an action.
% Actions in transactions are always ordered; order is significant.
% This works right for nested transactions, and use of macros.
% Transaction rights must all be satisfied upfront, before any action execution. (legal requirements)
% Transaction precondtitions must be satisfied after preceeding actions have been executed. (physical requirements)
% Succeeds once per reason!


% =====================================================================

allowed(god, _Action).

% =====================================================================

% Allowed because of holding an appropriate right.
allowed(Agent, Action) :-
	object(X),
	value(X, [(held_by, Agent), (instance_of, right(Action, Condition))]),
	call(Condition).

% Allowed because of owning an appropriate right.
allowed(Agent, Action) :-
	object(X),
	value(X, [(owned_by, Agent), (instance_of, right(Action, Condition))]),
	call(Condition).

% =====================================================================

% An accessible action is one that queries objects which the agent is allowed to query,
% and affects only objects owned by the agent. Only current state is supported.

accessible(Action, Agent) :-
	primitive(Agent, Action, _Preconditions, Effects),

	\+ (	member(Effect, Effects),
		decompose(Effect, Head, Body),
		(	create(X,_) = Head ; destroy(X) = Head  ; set(X,_,_) = Head 	),
		\+ (	object(X), value(X, owned_by, Agent)	)	),

	\+ (	member(Effect, Effects),
		decompose(Effect, Head, Body),
		(	object(X) = Body ; value(X,P,_) = Body ; (value(X,PVs) = Body, member((P,_), PVs))	),
		\+ (	allowed(Agent, query(X,P))	)	).

% =====================================================================
