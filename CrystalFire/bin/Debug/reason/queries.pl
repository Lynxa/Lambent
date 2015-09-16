% =========================================================================================================================
%	 						Simulation Submodule: Query Management
% =========================================================================================================================




% Reserves the following predicates:
% get_agent_view/2.


% The query/2 action is not defined explicitly in the domain.

% Can extend this, so as only objects that are near can be seen!


% =====================================================================
% Query Management Predicates
% =====================================================================

get_agent_view(Agent, StateView) :-
	state(CurrentState),
	findall((X, Properties), (
		objectin(X, CurrentState),
		findall((P, V), (
			valuein(X, P, V, CurrentState),
			once(allowed(Agent, query(X, P)))
		), Properties)
	), StateView).

% =====================================================================
