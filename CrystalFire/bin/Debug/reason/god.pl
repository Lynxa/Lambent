% =========================================================================================================================
%	 						Simulation Submodule: god Management
% =========================================================================================================================




% =====================================================================
% god Management Predicates
% =====================================================================

action(god, initialize) :- transaction([

	ActionCopy
	where (
		clause(initially(ActionList)),
		member(Action, ActionList),
		copy_term(Action, ActionCopy)
	)
]).


action(god, admit(Agent)) :- transaction([

	ActionCopy
	where (
		clause(on_entrance(Agent, ActionList)),
		member(Action, ActionList),
		copy_term(Action, ActionCopy)
	)
]).


action(god, dismiss(Agent)) :- transaction([

	ActionCopy
	where (
		clause(on_departure(Agent, ActionList)),
		member(Action, ActionList),
		copy_term(Action, ActionCopy)
	)
]).


action(god, enforce(Punishments)) :- transaction([

	Punishment 
	where member(Punishment, Punishments)

]).

% =====================================================================


