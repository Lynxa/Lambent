% =====================================================================


% This allows these predicates to be defined across multiple files (e.g., libraries).
:- dynamic classes/1, initially/1, on_entrance/2, on_departure/2, action/2.

:- op(500, xfy, where).

:- ["../utils/utils"].
:- [actions], [objects], [states], [database], 
   [allowed], [queries], [obligations], [god].
:- dynamic agents/1, agents_views/1.


show_detail([

%       action_execution_details,

       complete_state_objects,
       complete_state_properties,

%       simulation_outgoing_message,

        any_unspecified_messages

]).


% =====================================================================

simulation(CommunicationSimulationHost, CommunicationSimulationPort) :-

        Delay = 20,      % Maybe use event driven obligation checking.

        socket(internet, stream, CommunicationSimulationChannel),
        connect(CommunicationSimulationChannel, CommunicationSimulationHost/CommunicationSimulationPort),
        show(["\n"]),
        show(["[Simulation] ", "Established connection with <Communication Module>.\n"]),

        set_flag(variable_names, on),           % Ignores singleton variables when reading domains.

        once((
        repeat,
                show(["\n"]),
                show(["[Simulation] ", "Please enter the domain description filename: "]),
                read(Domain),
%Domain = "domains/english.pl", nl,
%Domain = "domains/coin.pl", nl,
        ((get_file_info(Domain, mode, _), compile("reason/stdlib"), compile(Domain)) -> true ; (
                show(["[Simulation] ", "Specified file failed to load properly.\n"]),
                fail
        ))
        )),

        set_flag(variable_names, check_singletons),


        show(["\n"]),
        show(["[Simulation] ", "Initializing simulation...\n"]),

        assert(agents([])),
        assert(agents_views([])),

        create_first_state,                             % Set up the initial state.
        process_message(msg(invoke(initialize), god)),

        show(["\n"]),
        show(["[Simulation] ", "Simulation initialized and ready for operation.\n"]),


        block(

                (repeat,

                        show(complete_state_objects, ["\n"]),
                        show(complete_state_objects, ["[Simulation] ", "Reporting god's and agents' view of current state.\n"]),
                        clause(agents(Agents)),
                        forall((
                                member(Agent, [god|Agents]),
                                get_agent_view(Agent, StateView),
                                OutMessage = report(view(Agent, state(StateView))),
                                send(CommunicationSimulationChannel, OutMessage),
                                show(simulation_outgoing_message, ["[Simulation] ", "Outgoing message: ", OutMessage, "\n"])
                        )),

                        (receive(CommunicationSimulationChannel, Delay, InMessage) ->
                        (
                                show(["\n"]),
                                show(["[Simulation] ", "Incoming message: ", InMessage, "\n"]),
                                create_new_state,
                                process_message(InMessage)
                        ) ; (
                                show(["\n"]),
                                show(["[Simulation] ", "Clock tick.\n"]),
                                create_new_state
                        )),

                        succeed((

                                \+ (\+ nonground(InMessage), InMessage = msg(shutdown, _)),

                                check_obligations(Punishments),
                                (Punishments = [] -> true ;
                                process_message(msg(invoke(enforce(Punishments)), god))),

                                show(["\n"]),
                                show(["[Simulation] ", "Forwarding observable state information to agents.\n"]),

/*      Send state views difference, in a sub-objects level.

                                retract(agents_views(AgentsViews)),

                                findall((Agent,StateView), (
                                        member((Agent,LastView), AgentsViews),
                                        get_agent_view(Agent, StateView),
                                        state_difference(LastView, StateView, DeletedObjects, DeletedProperties, UpdatedProperties),
                                        OutMessage = forward(Agent, state_update(DeletedObjects, DeletedProperties, UpdatedProperties)),
                                        send(CommunicationSimulationChannel, OutMessage),
                                        show(simulation_outgoing_message, ["[Simulation] ", "Outgoing message: ", OutMessage, "\n"])
                                ), AgentsNewViews),

                                assert(agents_views(AgentsNewViews))
*/

%/*     Send state views difference, using complete objects.

                                retract(agents_views(AgentsViews)),

                                findall((Agent,StateView), (
                                        member((Agent,LastView), AgentsViews),
                                        get_agent_view(Agent, StateView),
                                        subtract(LastView, StateView, Deleted),
                                        subtract(StateView, LastView, Added),
                                        OutMessage = forward(Agent, state_change(Deleted, Added)),
                                        send(CommunicationSimulationChannel, OutMessage),
                                        show(simulation_outgoing_message, ["[Simulation] ", "Outgoing message: ", OutMessage, "\n"])
                                ), AgentsNewViews),

                                assert(agents_views(AgentsNewViews))
%*/

/*      Send complete state views.

                                clause(agents(Agents)),

                                forall((
                                        member(Agent, Agents),
                                        get_agent_view(Agent, StateView),
                                        OutMessage = forward(Agent, state(StateView)),
                                        send(CommunicationSimulationChannel, OutMessage),
                                        show(simulation_outgoing_message, ["[Simulation] ", "Outgoing message: ", OutMessage, "\n"])
                                ))
*/
                        )),

                        (\+ nonground(InMessage), InMessage = msg(shutdown, _)),
                        close(CommunicationSimulationChannel)

                ),

        error, true).

% =====================================================================

process_message(msg(shutdown, _)) :-
        show(["\n"]),
        show(["[Simulation] ", "Shutting down...\n"]).
        % Last state is properly written in the DB.

process_message(msg(join(NewAgent), _)) :-
        process_message(msg(invoke(admit(NewAgent)), god)),
        retract(agents(Agents)),
        assert(agents([NewAgent|Agents])),
        retract(agents_views(AgentsViews)),
        assert(agents_views([(NewAgent,[])|AgentsViews])),
        show(["[Simulation] ", "Agent joined: ", NewAgent, "\n"]).

process_message(msg(leave(OldAgent), _)) :-
        process_message(msg(invoke(dismiss(OldAgent)), god)),
        retract(agents(Agents)),
        delete(OldAgent, Agents, RestAgents),
        assert(agents(RestAgents)),
        retract(agents_views(AgentsViews)),
        delete((OldAgent,_), AgentsViews, RestAgentsViews),
        assert(agents_views(RestAgentsViews)),
        show(["[Simulation] ", "Agent left: ", OldAgent, "\n"]).

process_message(msg(invoke(Action), Agent)) :-
        process_action(Action, Agent).

% =====================================================================
