% =========================================================================================================================
%	 							Standard Library
% =========================================================================================================================

% Each item in the list is locally universally quantified.

% Preconditions and effects do not share free variables. And they shouldn't. An action's effects do not
% depend on its executability preconditions.

% =====================================================================
% Class Specification
% =====================================================================

% For items that are just quantity, but not entities per se, we can have container classes that describe the
% item and the quantity. See account class.


classes([

	(event, [
		description,
		happened_at,
		expired_at
	]),

	(right(Action, Condition), [
		owned_by,
		held_by
	]),

	(obligation(Satisfy, Violate, Punishment), [
		owned_by,
		held_by,
		status
	]),

	(variable, [
		owned_by,
		value
	]),

	(account, [
		owned_by,
		held_by,
		amount
	])

]).


% =====================================================================
% Initial Specification
% =====================================================================

initially([]).


% =====================================================================
% On Entrance Specification
% =====================================================================

on_entrance(Agent, [

% Agents are given accounts. Since they own their accounts, they can give ownership
% to others. In order for other agents to use those money, they should first transfer
%them in their account.

	transaction([
	        create_object(account(Agent), account),
	        set_properties(account(Agent), [(amount, 0)]),
	        give(account(Agent), Agent)
	]),

% Agents own the right to actions depending / affecting only objects they can query / own.
	issue(right(Action, (
			accessible(Action, Agent)
		)), Agent),

% Agents own the right to query objects they own.
	issue(right(query(X, P), (
			object(X),
			value(X, [(owned_by, Agent), (P, V)])
		)), Agent),

% Agents own the right to query rights they hold.
	issue(right(query(X, P), (
			object(X),
			value(X, [(instance_of, right(Action, Precondition)), (held_by, Agent), (P, V)])
		)), Agent),

% Agents own the right to query obligations they hold.
	issue(right(query(X, P), (
			object(X),
			value(X, [(instance_of, obligation(Satisfy, Violate, Punishment)), (held_by, Agent), (P, V)])
		)), Agent),

% Agents own the right to query objects that are events.
% That is: Agents can see actions taking place, and time passing by.
	issue(right(query(X, P), (
			object(X),
			value(X, [(instance_of, event), (P, V)])
		)), Agent),

% Agents own the right to issue ownership of rights to actions for which they own the right to execute.
% Conditions of former right should be at least as strict as for the latter right.
	issue(right(issue_o(right(Action, (Condition, ExtraCondition)), SomeAgent), (
			object(X),
			value(X, [(owned_by, Agent), (instance_of, right(Action, Condition))])
		)), Agent),

% Agents own the right to issue possession of rights to actions for which they own the right to execute.
% Conditions of former right should be at least as strict as for the latter right.
	issue(right(issue_p(right(Action, (Condition, ExtraCondition)), SomeAgent), (
			object(X),
			value(X, [(owned_by, Agent), (instance_of, right(Action, Condition))])
		)), Agent),

% Agents own the right to use objects they hold.
	issue(right(Action, (
			object(X),
			value(X, [(held_by, Agent), (uses, Uses)]),
			member(Action, Uses)
		)), Agent),

% Agents own the right to on obligations, with punishment something being taken away.
% What if the agent does not have that object when it is time to get punished?
% Or even when it makes the promise?
% This is okay, because other agents might not trust the former agent when it makes such promises.
	issue(right(take_on(obligation(Satisfy, Violate, jail(Agent))), true), Agent),

% Agents own the right to create variables.
	issue(right(create_var(Variable), true), Agent)

]).


% =====================================================================
% On Departure Specification
% =====================================================================

on_departure(Agent, [

% What was owned by departing agent, is now owned by god.
	take(X, Agent)
	where (
		object(X),
		value(X, owned_by, Agent)
	),

% What was held by departing agent, is now held by god.
	take_p(X, Agent)
	where (
		object(X),
		value(X, held_by, Agent)
	)

]).


% =====================================================================
% Action Specification
% =====================================================================


% You don't have the a priori right to give money to other people.

% =====================================================================

action(Agent, create_object(X, Class)) :-

	preconditions([
		Agent = god,	% It is physically impossible to create objects!
		\+ object(X),
		class(Class)	
	]),

	effects([
		create(X, Class),
		set(X, owned_by, Agent),
		set(X, held_by, Agent)
	]).


action(Agent, set_properties(X, Properties)) :-

	preconditions([
		Agent = god,	% It is physically impossible to set object properties!
		object(X)
	]),

	effects([
		set(X, P, V) where member((P,V), Properties)
	]).

% =====================================================================

action(Agent, create_var(Variable)) :-

	preconditions([
		\+ object(Variable)
	]),

	effects([
		create(Variable, variable),
		set(Variable, owned_by, Agent)
	]).

% =====================================================================

action(Agent, set_var(Variable, Value)) :-

	preconditions([
		object(Variable)
	]),

	effects([
		set(Variable, value, Value)
	]).

% =====================================================================

% Does not necessarily transfer possession as well.

action(Agent, give_o(Item, SomeAgent)) :-

	preconditions([]),

	effects([
		set(Item, owned_by, SomeAgent)
	]).


action(Agent, give_p(Item, SomeAgent)) :-

	preconditions([
		object(Item),
		value(Item, held_by, Agent)
	]),

	effects([
		set(Item, held_by, SomeAgent)
	]).


action(Agent, give(Item, SomeAgent)) :-

	transaction([
		give_p(Item, SomeAgent),
		give_o(Item, SomeAgent)
	]).

% =====================================================================

% Transactions can be used in this simple way as macros.

action(Agent, give_up_o(Item)) :-

	transaction([
		give_o(Item, god)
	]).


action(Agent, give_up_p(Item)) :-

	transaction([
		give_p(Item, god)
	]).


action(Agent, give_up(Item)) :-

	transaction([
		give(Item, god)
	]).

% =====================================================================

% Both agents should have accounts for giving money.

action(Agent, give_money(Amount, SomeAgent)) :-

	preconditions([
		object(account(Agent)),
		value(account(Agent), [(owned_by, Agent), (instance_of, account), (amount, AccountAmount)]),
		atleast(AccountAmount, Amount),

		object(account(SomeAgent)),
		value(account(SomeAgent), [(owned_by, SomeAgent), (instance_of, account)])
	]),

	effects([
		set(account(Agent), amount, NewAccountAmount1)
		where (
			value(account(Agent), amount, AccountAmount1),
			NewAccountAmount1 is AccountAmount1 - Amount
		),
		set(account(SomeAgent), amount, NewAccountAmount2)
		where (
			value(account(SomeAgent), amount, AccountAmount2),
			NewAccountAmount2 is AccountAmount2 + Amount
		)
	]).

% =====================================================================

action(Agent, issue_o(right(Action, Conditions), SomeAgent)) :-

	preconditions([
		class(right(Action, Conditions))
	]),

	effects([
		create(right(ID), right(Action, Conditions)),
		set(right(ID), owned_by, SomeAgent),
		set(right(ID), held_by, Agent)
	]).


action(Agent, issue_p(right(Action, Conditions), SomeAgent)) :-

	preconditions([
		class(right(Action, Conditions))
	]),

	effects([
		create(right(ID), right(Action, Conditions)),
		set(right(ID), owned_by, Agent),
		set(right(ID), held_by, SomeAgent)
	]).


action(Agent, issue(right(Action, Conditions), SomeAgent)) :-

	preconditions([
		class(right(Action, Conditions))
	]),

	effects([
		create(right(ID), right(Action, Conditions)),
		set(right(ID), owned_by, SomeAgent),
		set(right(ID), held_by, SomeAgent)
	]).


% =====================================================================

% Does not necessarily transfer possession as well.

action(Agent, take_o(Item, SomeAgent)) :-

	preconditions([
		object(Item),
		value(Item, owned_by, SomeAgent)
	]),

	effects([
		set(Item, owned_by, Agent)
	]).


action(Agent, take_p(Item, SomeAgent)) :-

	preconditions([
		object(Item),
		value(Item, held_by, SomeAgent)
	]),

	effects([
		set(Item, held_by, Agent)
	]).


% If an agent can take ownership, it then gains the right to take possession.
action(Agent, take(Item, SomeAgent)) :-

	transaction([
		take_o(Item, SomeAgent),
		take_p(Item, SomeAgent)
	]).

% =====================================================================

action(Agent, take_on(obligation(Satisfy, Violate, Punishment))) :-

	preconditions([
		class(obligation(Satisfy, Violate, Punishment))
	]),

	effects([
		create(obligation(ID), obligation(Satisfy, Violate, Punishment)),
		set(obligation(ID), owned_by, god),
		set(obligation(ID), held_by, Agent)
	]).

% =====================================================================

% Both agents should have accounts for taking money.

action(Agent, take_money(Amount, SomeAgent)) :-

	preconditions([
		object(account(SomeAgent)),
		value(account(SomeAgent), [(owned_by, SomeAgent), (instance_of, account), (amount, AccountAmount)]),
		atleast(AccountAmount, Amount),

		object(account(Agent)),
		value(account(Agent), [(owned_by, Agent), (instance_of, account)])
	]),

	effects([
		set(account(SomeAgent), amount, NewAccountAmount1)
		where (
			value(account(SomeAgent), amount, AccountAmount1),
			NewAccountAmount1 is AccountAmount1 - Amount
		),
		set(account(Agent), amount, NewAccountAmount2)
		where (
			value(account(Agent), amount, AccountAmount2),
			NewAccountAmount2 is AccountAmount2 + Amount
		)
	]).

% =====================================================================

% Publish information.

action(Agent, publish(Variable, Value)) :-

	transaction([
		create_var(Variable),
		set_var(Variable, Value),
		issue(right(query(Variable, value), true), SomeAgent)
	]).

% =====================================================================
% This is the end of the domain description. Do not remove this line! #
% =====================================================================
