% =====================================================================

:- ["../utils/utils"].
:- dynamic dispatch_channels/1, to_close/1, simulation_channel/1, observation_channel/1.


show_detail([

        communication_incoming_message_notification,
%        communication_incoming_message,

        communication_forwarded_message_notification,
%        communication_forwarded_message,

        any_unspecified_messages

]).


% =====================================================================

communication(AdministrationPort, ObservationPort, RegistrationPort, SimulationPort) :-

        socket(internet, stream, AdministrationSocket),
        bind(AdministrationSocket, AdministrationHost/AdministrationPort),
        show(["\n"]),
        show(["[Communication] ", "Waiting for (Administration) link at: ", AdministrationHost/AdministrationPort, "\n"]),
        listen(AdministrationSocket, 1), accept(AdministrationSocket, _, AdministrationChannel),
        show(["[Communication] ", "Link for (Administration) established via channel: ", AdministrationChannel, "\n"]),

        socket(internet, stream, ObservationSocket),
        bind(ObservationSocket, ObservationHost/ObservationPort),
        show(["\n"]),
        show(["[Communication] ", "Waiting for (Observation) link at: ", ObservationHost/ObservationPort, "\n"]),
        listen(ObservationSocket, 1), accept(ObservationSocket, _, ObservationChannel),
        show(["[Communication] ", "Link for (Observation) established via channel: ", ObservationChannel, "\n"]),

        socket(internet, stream, RegistrationSocket),
        bind(RegistrationSocket, RegistrationHost/RegistrationPort),
        show(["\n"]),
        show(["[Communication] ", "Waiting for (Registration) link at: ", RegistrationHost/RegistrationPort, "\n"]),
        listen(RegistrationSocket, 1), accept(RegistrationSocket, _, RegistrationChannel),
        show(["[Communication] ", "Link for (Registration) established via channel: ", RegistrationChannel, "\n"]),

        socket(internet, stream, SimulationSocket),
        bind(SimulationSocket, SimulationHost/SimulationPort),
        show(["\n"]),
        show(["[Communication] ", "Waiting for (Simulation) link at: ", SimulationHost/SimulationPort, "\n"]),
        listen(SimulationSocket, 1), accept(SimulationSocket, _, SimulationChannel),
        show(["[Communication] ", "Link for (Simulation) established via channel: ", SimulationChannel, "\n"]),

        show(["\n"]),
        show(["[Communication] ", "Ready for operation...\n"]),

        assert(dispatch_channels([])),
        assert(to_close([])),
        assert(simulation_channel(SimulationChannel)),
        assert(observation_channel(ObservationChannel)), % Output only channel.

        block(

                (repeat,
                        succeed(read_channel(AdministrationChannel, 1)),
                        succeed(read_channel(RegistrationChannel, 2)),
                        succeed(read_channel(SimulationChannel, 3)),
                        cycle_channels(DispatchChannel),
                        succeed(read_channel(DispatchChannel, 4)),
                        sleep(0.1), % To alleviate the problem of continuous loops.
                fail),

        shutdown, true).

% =====================================================================

cycle_channels(DispatchChannel) :-
        retract(dispatch_channels([DispatchChannel|DispatchChannels])), 
        append(DispatchChannels, [DispatchChannel], CycledDispatchChannels),
        assert(dispatch_channels(CycledDispatchChannels)), !. 

% =====================================================================

read_channel(Channel, AuthorityLevel) :-
        receive(Channel, Message),
        show(communication_incoming_message_notification, ["\n"]),
        show(communication_incoming_message_notification, ["[Communication] ", "Message received from (Channel ", Channel, "): (suppressed)\n"]),
        show(communication_incoming_message, ["\n"]),
        show(communication_incoming_message, ["[Communication] ", "Message received from (Channel ", Channel, "): ", Message, "\n"]),
        once((process_message(Channel, Message, AuthorityLevel) ;
                show(["[Communication] ", "Further message processing halted.\n"]))).

% =====================================================================

delete_channel(Channel) :-
        retract(to_close(ClosedChannels)),
        assert(to_close([Channel|ClosedChannels])),
        retract(dispatch_channels(DispatchChannels)),
        delete(Channel, DispatchChannels, RestDispatchChannels),
        assert(dispatch_channels(RestDispatchChannels)).

% =====================================================================

% Someone disconnected without properly informing the communication module.
% Should remove Sender from channels!
% Authority level is treated here as an importance level.
process_message(Sender, end_of_file, AuthorityLevel) :-
        (AuthorityLevel == 1 ; AuthorityLevel == 2 ; AuthorityLevel == 3), !,
        show(["[Communication] ", "Connection abnormally terminated for system channel: ", Sender, "\n"]),
        show(["[Communication] ", "Unable to recover from this error. System will shutdown.\n"]),
        process_message(Sender, shutdown, 0).

process_message(Sender, end_of_file, AuthorityLevel) :-
        AuthorityLevel == 4, !,
        show(["[Communication] ", "Connection abnormally terminated for dispatch channel: ", Sender, "\n"]),
        show(["[Communication] ", "Further communication with channel will be terminated.\n"]),
        delete_channel(Sender).
        % Should the agent remain in the simulation? For now we say "yes".

% =====================================================================

% Authority level 0 is reserved for unrecoverable system errors.
% Sender is the channel that caused the error in the first place.
% Only agents, simulation, and observation are informed of shutdown.
% At least the participants are informed, and the simulation closes normally.
process_message(Sender, shutdown, AuthorityLevel) :-
        (AuthorityLevel == 0 ; AuthorityLevel == 1), !,
        clause(dispatch_channels(DispatchChannels)),
        forall((
                member(DispatchChannel, DispatchChannels), % Disconnect all participants.
                process_message(Sender, disconnect(DispatchChannel), AuthorityLevel)
        )),
        retract(dispatch_channels(_)),
        clause(to_close(ClosedChannels)),
        forall((
                member(ClosedChannel, ClosedChannels), % Close all channels.
                close(ClosedChannel),
                show(["[Communication] ", "Channel closed: ", ClosedChannel, "\n"]),
                show(["\n"])
        )),
        retract(to_close(_)),

        clause(observation_channel(ObservationChannel)),
        process_message(Sender, report(disconnected), AuthorityLevel), % Disconnect observation.
        retract(observation_channel(_)),
        close(ObservationChannel),
        show(["[Communication] ", "Channel closed: ", ObservationChannel, "\n"]),
        show(["\n"]),

        clause(simulation_channel(SimulationChannel)),
        (Sender \== SimulationChannel -> (
                process_message(Sender, submit(shutdown), AuthorityLevel) % Inform simulation about shutdown and close channel.
        ) ; true), % If the simulation module initiated the shutdown, then we should not inform it.
        retract(simulation_channel(_)),
        close(SimulationChannel),
        show(["[Communication] ", "Channel closed: ", SimulationChannel, "\n"]),
        show(["\n"]),

        show(["[Communication] ", "Shutting down...\n"]),
        exit_block(shutdown).

process_message(_Sender, shutdown, _AuthorityLevel) :-
        show(["[Communication] ", "Message requires authority level '1' to be processed!\n"]),
        !, fail.

% =====================================================================

process_message(Sender, connect(ClientHostPort), AuthorityLevel) :-
        (AuthorityLevel == 0 ; AuthorityLevel == 1 ; AuthorityLevel == 2), !,
        socket(internet, stream, NewDispatchChannel),
        connect(NewDispatchChannel, ClientHostPort),
        retract(dispatch_channels(DispatchChannels)), 
        append(DispatchChannels, [NewDispatchChannel], NewDispatchChannels),
        assert(dispatch_channels(NewDispatchChannels)),
        send(NewDispatchChannel, msg(connected(NewDispatchChannel), Sender)),
        show(["[Communication] ", "Link with client established via channel: ", NewDispatchChannel, "\n"]),
        clause(simulation_channel(SimulationChannel)), 
        send(SimulationChannel, msg(join(NewDispatchChannel), Sender)),
        show(["[Communication] ", "Informed (Channel ", SimulationChannel, ") of established connection.\n"]).

process_message(_Sender, connect(_ClientHostPort), _AuthorityLevel) :-
        show(["[Communication] ", "Message requires authority level '1' or '2' to be processed!\n"]),
        !, fail.

% =====================================================================

process_message(Sender, disconnect(OldDispatchChannel), AuthorityLevel) :-
        (AuthorityLevel == 0 ; AuthorityLevel == 1 ; AuthorityLevel == 2), !,
        ((
                clause(dispatch_channels(DispatchChannels)),
                member(OldDispatchChannel, DispatchChannels),
                send(OldDispatchChannel, msg(disconnected, Sender)),
                show(["[Communication] ", "Informed (Channel ", OldDispatchChannel, ") of connection termination.\n"]),
                clause(simulation_channel(SimulationChannel)),
                (Sender \== SimulationChannel -> (
                        send(SimulationChannel, msg(leave(OldDispatchChannel), Sender)),
                        show(["[Communication] ", "Informed (Channel ", SimulationChannel, ") of terminated connection.\n"])
                ) ; true), % If the simulation module initiated the disconnection, then we should not inform it.
                % close(OldDispatchChannel), % Do not close, so as to avoid reuse of channel number.
                delete_channel(OldDispatchChannel),
                show(["[Communication] ", "Channel closed: ", OldDispatchChannel, "\n"])
        )
        ;
        show(["[Communication] ", "Could not close channel: ", OldDispatchChannel, "\n"])),
        !.

process_message(_Sender, disconnect(_OldDispatchChannel), _AuthorityLevel) :-
        show(["[Communication] ", "Message requires authority level '1' or '2' to be processed!\n"]),
        !, fail.

% =====================================================================

process_message(Sender, report(Body), AuthorityLevel) :-
        (AuthorityLevel == 0 ; AuthorityLevel == 1 ; AuthorityLevel == 2 ; AuthorityLevel == 3), !,
        clause(observation_channel(ObservationChannel)),
        send(ObservationChannel, msg(Body, Sender)),
        send(ObservationChannel,"\n"),
        show(["[Communication] ", "Forwarded message: ", msg(Body, Sender), "\n"]),
        show(["[Communication] ", "Channel receiving message: ", ObservationChannel, "\n"]).

process_message(_Sender, report(_Body), _AuthorityLevel) :-
        show(["[Communication] ", "Message requires authority level '1', '2', or '3' to be processed!\n"]),
        !, fail.

% =====================================================================

process_message(Sender, forward(Recipient, Body), AuthorityLevel) :-
        (AuthorityLevel == 0 ; AuthorityLevel == 1 ; AuthorityLevel == 2 ; AuthorityLevel == 3), !,
        clause(dispatch_channels(DispatchChannels)),
        show(communication_forwarded_message_notification, ["[Communication] ", "Forwarded message: (suppressed) \n"]),
        show(communication_forwarded_message, ["[Communication] ", "Forwarded message: ", msg(Body, Sender), "\n"]),
        show(["[Communication] ", "Channels receiving message: "]),
        findall(_, (
                        member(Recipient, DispatchChannels), % Checks if Recipient exists!
                        send(Recipient, msg(Body, Sender)),
                        show([Recipient, ", "])
                   ),
               _),
        show([".\n"]).

process_message(_Sender, forward(_Recipient, _Body), _AuthorityLevel) :-
        show(["[Communication] ", "Message requires authority level '1', '2', or '3' to be processed!\n"]),
        !, fail.

% =====================================================================

% This only makes sense for dispatch channels.
% We leave all channels here for consistency of authority levels.
process_message(Sender, leave, AuthorityLevel) :-
        (AuthorityLevel == 0 ; AuthorityLevel == 1 ; AuthorityLevel == 2 ; AuthorityLevel == 3 ; AuthorityLevel == 4), !,
        ((
                % close(Sender), % Do not close, so as to avoid reuse of channel number.
                delete_channel(Sender),
                show(["[Communication] ", "Channel closed following sender's request: ", Sender, "\n"]),
                clause(simulation_channel(SimulationChannel)),
                send(SimulationChannel, msg(leave(Sender), Sender)),
                show(["[Communication] ", "Informed (Channel ", SimulationChannel, ") of terminated connection.\n"])
        )
        ;
        show(["[Communication] ", "Could not close channel: ", Sender, "\n"])),
        !.

process_message(_Sender, leave, _AuthorityLevel) :-
        show(["[Communication] ", "Message requires authority level '1', '2', '3', or '4' to be processed!\n"]),
        !, fail.

% =====================================================================

process_message(Sender, submit(Body), AuthorityLevel) :-
        (AuthorityLevel == 0 ; AuthorityLevel == 1 ; AuthorityLevel == 2 ; AuthorityLevel == 3 ; AuthorityLevel == 4), !,
        clause(simulation_channel(SimulationChannel)),
        send(SimulationChannel, msg(Body, Sender)),
        show(["[Communication] ", "Forwarded message: ", msg(Body, Sender), "\n"]),
        show(["[Communication] ", "Channel receiving message: ", SimulationChannel, "\n"]).

process_message(_Sender, submit(_Body), _AuthorityLevel) :-
        show(["[Communication] ", "Message requires authority level '1', '2', '3', or '4' to be processed!\n"]),
        !, fail.

% =====================================================================

process_message(_, _, _) :-
        show(["[Communication] ", "Message type not supported!\n"]),
        !, fail.

% =====================================================================
