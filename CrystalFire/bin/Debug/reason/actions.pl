% =========================================================================================================================
%                                                       Simulation Submodule: Action Module
% =========================================================================================================================




% =====================================================================

process_action(Action, Agent) :-
        % check rights and execute action. carefull about transactions.

        show(["\n"]),
        show(["[Kernel] ", "Invoked action: ", Action, "\n"]),                          % present action, ...
        show(["[Kernel] ", "Agent invoking action: ", Agent, "\n"]),                    % ... and agent invoking it.

        once((\+ nonground(Action)) ;   (                                                               % check if action is instantiated.
                nonground(Action, L),
                show(["[Kernel] ", "Variable '", L, "' not grounded. Action ignored.\n"]),
                fail)                                                                                           % if not, print error message and fail.
        ),

        record_invokation(Agent, Action, Event),                                                        % record action invokation.
        backup_state(BackupState),                                                                      % backup current state.

        show(["[Kernel] ", "Attempting to execute invoked action.\n"]),
        (execute(Action, Agent, no) -> (
                record_outcome(Event, successfully),
                show(["[Kernel] ", "Invoked action successfully executed. Simulation state updated.\n"])
        ) ; (
                restore_state(BackupState),
                record_outcome(Event, unsuccessfully),
                show(["[Kernel] ", "Invoked action failed to execute. Simulation state rolled back.\n"])
        )).

% =====================================================================

record_invokation(Agent, Action, event(ID)) :-
        value(clock, happened_at, Time),
        create(event(ID), event),
        set(event(ID), description, invoked(Agent, Action, pending)),
        set(event(ID), happened_at, Time),
        set(event(ID), expired_at, Time).

record_outcome(Event, Outcome) :-
        object(Event),
        value(Event, description, invoked(Agent, Action, pending)),
        set(Event, description, invoked(Agent, Action, Outcome)).

% =====================================================================

execute(Action, Agent, HasInheritedRight) :-

        show(action_execution_details, ["[Kernel] ", "Attempting to execute action: ", Action, "\n"]),

        ((decompose(Action, Head, Body), Body \== true) -> (
                show(action_execution_details, ["[Kernel] ", "Expanded quantified action to grounded instances.\n"]),
                foreach(Body, execute(Head, Agent, HasInheritedRight))
        ) ; (

        has_right(Action, Agent, HasInheritedRight, HasRight),

        (primitive(Agent, Action, _, _) -> (
                show(action_execution_details, ["[Kernel] ", "Expanded primitive action to preconditions and effects.\n"]),
                execute_primitive(Action, Agent, HasRight)
        ) ;

        (macro(Agent, Action, Transaction) -> (
                show(action_execution_details, ["[Kernel] ", "Expanded macro to transaction: ", Transaction, "\n"]),
                execute(Transaction, Agent, HasRight)
        ) ;

        (transaction(Action, ActionList) -> (
                show(action_execution_details, ["[Kernel] ", "Expanded transaction to action list: ", ActionList, "\n"]),
                execute_list(ActionList, Agent, HasRight)
        ) ;

        show(action_execution_details, ["[Kernel] ", "Action not recognized as primitive, macro, or transaction.\n"]),
        fail
        )))

        )).


execute_primitive(Action, Agent, HasRight) :-

        (HasRight == "yes" -> (
                true
        ) ; (
                show(action_execution_details, ["[Kernel] ", "Primitive action cannot be executed without appropriate right.\n"]),
                fail
        )),

        primitive(Agent, Action, Preconditions, Effects),
        listtoterm(Preconditions,OnePrecondition),
        listtoterm(Effects,OneEffect),

        show(action_execution_details, ["[Kernel] ", "Checking action's preconditions: "]),
        (call(OnePrecondition) -> (
                show(action_execution_details, ["passed.\n"])
        ) ; (
                show(action_execution_details, ["failed.\n"]),
                show(action_execution_details, ["[Kernel] ", "Primitive action cannot be executed without meeting appropriate preconditions.\n"]),
                fail
        )),

        show(action_execution_details, ["[Kernel] ", "Executing primitive action to produce its effects.\n"]),
        call(OneEffect).


execute_list([], _Agent, _HasInheritedRight).
execute_list([H|B], Agent, HasInheritedRight) :-
        execute(H, Agent, HasInheritedRight),
        execute_list(B, Agent, HasInheritedRight).


Effect where Condition :- foreach(Condition, Effect).

% =====================================================================

has_right(Action, Agent, HasInheritedRight, HasRight) :-
        (HasInheritedRight == "yes" -> (
                show(action_execution_details, ["[Kernel] ", "Execution right inherited from parent transaction.\n"]),
                HasRight = "yes"
        ) ; (
                show(action_execution_details, ["[Kernel] ", "Checking for appropriate right: "]),
                (allowed(Agent, Action) -> (
                        show(action_execution_details, ["passed.\n"]),
                        HasRight = "yes"
                ) ; (
                        show(action_execution_details, ["failed.\n"]),
                        HasRight = "no"
                ))
        )).

% =====================================================================

primitive(Agent, Action, Preconditions, Effects) :-
        clause(action(Agent, Action) :- (preconditions(Preconditions), effects(Effects))).              % primitive action.

macro(Agent, Action, transaction(ActionList)) :-
        clause(action(Agent, Action) :- transaction(ActionList)).                                       % macro definition.

transaction(transaction(ActionList), ActionList).                                                       % transaction.

% =====================================================================

decompose(Where, Head, Body) :-
        once(((Head where Body = Where) ; (Head = Where, Body = true))).

% =====================================================================
