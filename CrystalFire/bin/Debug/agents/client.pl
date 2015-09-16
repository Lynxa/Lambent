% =====================================================================

:- [tools].
:- dynamic server_channel/1.



show_detail([

	client_incoming_message_notification,
%	client_incoming_message,

%	agent_state_objects,
%	agent_state_properties,

	any_unspecified_messages

]).


% =====================================================================

client(ServerHost, ServerPort) :-

	socket(internet, stream, RegistrationSocket),
	connect(RegistrationSocket, ServerHost/ServerPort),
	show(["\n"]),
	show(["[Client] ", "Registered with <Registration Module>.\n"]),

	show(["[Client] ", "Sending contact information.\n"]),
	socket(internet, stream, CommunicationSocket), bind(CommunicationSocket, ClientHostPort),
	send(RegistrationSocket, ClientHostPort), close(RegistrationSocket),
	listen(CommunicationSocket, 1), accept(CommunicationSocket, _, ServerChannel),
	assert(server_channel(ServerChannel)),
	show(["[Client] ", "Established connection with <Communication Module>.\n"]),

	repeat,

		succeed(check_messages), % Get next pending message.
		sleep(1), % To alleviate the problem of continuous loops.
		succeed(take_decision(Action)),
		succeed((
			\+nonground(Action), Action \== disconnected,
			((Action == leave, OutMessage = leave) ;
			  OutMessage = submit(invoke(Action))),
		    	send(ServerChannel, OutMessage),
			show(["\n"]),
			show(["[Client] ", "Outgoing message: ", OutMessage, "\n"]),
			(Action \== leave -> (repeat, check_messages) ;  true)
				% If agent is not leaving,
				% wait until client receives feedback.
				% Avoids repeated action prompting.
		)),

	((Action == disconnected) ; (Action == leave)), % This condition is not met if take_decision/1 fails.
	close(ServerChannel),
	show(["\n"]),
	show(["[Client] ", "Shutting down...\n"]).

% =====================================================================

check_messages :-
	clause(server_channel(ServerChannel)),
	once(receive(ServerChannel, InMessage)),
	show(client_incoming_message_notification, ["\n"]),
	show(client_incoming_message_notification, ["[Client] ", "Incoming message: (suppressed)\n"]),
	show(client_incoming_message, ["\n"]),
	show(client_incoming_message, ["[Client] ", "Incoming message: ", InMessage, "\n"]),
	process_message(InMessage).

% =====================================================================
