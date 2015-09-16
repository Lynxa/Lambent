% =====================================================================

:- [client].
:- dynamic user_channel/1, state/1, name/1.


show_detail([

        agent_state_objects,
        agent_state_properties,

        any_unspecified_messages

]).


% =====================================================================

proxy(ServerHost, ServerPort, ProxyHost, ProxyPort) :-

        ProxyHostPort = ProxyHost / ProxyPort,

        assert(state([])), assert(name(unknown)), randomise,

        socket(internet, stream, ProxySocket), bind(ProxySocket, ProxyHostPort),
        show(["\n"]),
        show(["[Proxy] ", "Waiting for user connection at: ", ProxyHostPort, "\n"]),
        listen(ProxySocket, 1), accept(ProxySocket, _UserHostPort, UserChannel),
        assert(user_channel(UserChannel)),
        show(["\n"]),
        show(["[Proxy] ", "Established connection with user.\n"]),

        show(["\n"]),
        show(["[Proxy] ", "Joining simulation...\n"]),
        client(ServerHost, ServerPort),
        show(["\n"]),
        show(["[Proxy] ", "Leaving simulation...\n"]),

        close(ProxySocket),
        show(["\n"]),
        show(["[Proxy] ", "Closed connection with user.\n"]).

% =====================================================================

process_message(msg(connected(Name), _)) :-
        !, retract(name(_)), assert(name(Name)),
        show(["\n"]),
        show(["[Proxy] ", "Connected to the simulation under the name: ", Name, "\n"]).

process_message(msg(state(State), _)) :-
        !, retract(state(_)), assert(state(State)),
        show(["\n"]),
        show(["[Proxy] ", "Updated local copy of the simulation state.\n"]),
        view_state(State).

process_message(msg(state_change(Deleted, Added), _)) :-
        !, retract(state(OldState)),
        subtract(OldState, Deleted, Unchanged),
        union(Unchanged, Added, NewState),
        assert(state(NewState)),
        show(["\n"]),
        show(["[Proxy] ", "Updated local copy of the simulation state.\n"]),
        view_state(NewState).

process_message(msg(state_update(DeletedObjects, DeletedProperties, UpdatedProperties), _)) :-
        !, retract(state(OldState)),
        state_reconstruct(OldState, DeletedObjects, DeletedProperties, UpdatedProperties, NewState),
        assert(state(NewState)),
        show(["\n"]),
        show(["[Agent] ", "I'd better update my local copy of the simulation state.\n"]),
        view_state(NewState).

process_message(msg(disconnected, _)) :-
        !, retract(name(_)), assert(name(disconnected)),
        retract(state(_)), assert(state([])),
        show(["\n"]),
        show(["[Proxy] ", "Disconnected from the simulation!\n"]).

process_message(_) :-
        show(["\n"]),
        show(["[Proxy] ", "Unknown message received.\n"]).

% =====================================================================

take_decision(disconnected) :-
        retract(name(disconnected)), assert(name(unknown)).

take_decision(UserSelectedAction) :-
        clause(user_channel(UserChannel)),
        receive(UserChannel, UserSelectedAction),
        show(["\n"]),
        show(["[Proxy] ", "Submitted action: ", UserSelectedAction, "\n"]).

% =====================================================================
