% =========================================================================================================================
%	 						Simulation Submodule: Obligation Management
% =========================================================================================================================


% All obligations are tested first, and then the punishments are enforced.

% Reserves the following predicates:
% check_obligations/1.



% =====================================================================
% Obligation Management Predicates
% =====================================================================

check_obligations(Punishments) :-
	show(["\n"]),
	show(["[Kernel] ", "Checking obligations for satisfaction or violation.\n"]),
	findall(Punishment, (
		pending(Agent, X, Satisfy, Violate, Punishment),
		\+ satisfied(Agent, X, Satisfy, Violate, Punishment),
                violated(Agent, X, Satisfy, Violate, Punishment)
        ), Punishments).


pending(Agent, X, Satisfy, Violate, Punishment) :-
	object(X),
	value(X, held_by, Agent),
	value(X, instance_of, obligation(Satisfy, Violate, Punishment)),
	value(X, status, undefined).

satisfied(Agent, X, Satisfy, Violate, Punishment) :-
	call(Satisfy),
	value(clock, happened_at, Time),
	set(X, status, satisfied_at(Time)),
	show(["[Kernel] ", obligation(Satisfy, Violate, Punishment), " satisfied by ", Agent, ".\n"]).

violated(Agent, X, Satisfy, Violate, Punishment) :-
	call(Violate),
	value(clock, happened_at, Time),
	set(X, status, violated_at(Time)),
	show(["[Kernel] ", obligation(Satisfy, Violate, Punishment), " violated by ", Agent, ".\n"]).

% =====================================================================


