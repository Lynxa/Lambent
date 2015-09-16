% =====================================================================

:- ["../utils/utils"].

% =====================================================================

registration(CommunicationManagementHost, CommunicationManagementPort, RegistrationHost, RegistrationPort) :-

	CommunicationManagementHostPort = CommunicationManagementHost/CommunicationManagementPort,
	RegistrationHostPort = RegistrationHost/RegistrationPort,

	socket(internet, stream, CommunicationManagementChannel),
	connect(CommunicationManagementChannel, CommunicationManagementHostPort),
	show(["\n"]),
	show(["[Registration] ", "Established connection with <Communication Module>.\n"]),

	socket(internet, stream, RegistrationSocket), bind(RegistrationSocket, RegistrationHostPort),
	show(["\n"]),
	show(["[Registration] ", "Waiting for client registration at: ", RegistrationHostPort, "\n"]),
	repeat,
		listen(RegistrationSocket, 1), accept(RegistrationSocket, ClientHostPort, ClientStream),
		show(["\n"]),
		show(["[Registration] ", "Registration requested by: ", ClientHostPort, "\n"]),
		select([ClientStream], block, [ClientStream]),
		read(ClientStream, ClientHostPort2), close(ClientStream),
		show(["[Registration] ", "Contact information received.\n"]),
		send(CommunicationManagementChannel, connect(ClientHostPort2)),
		show(["[Registration] ", "Command sent to <Communication Module>: ", connect(ClientHostPort2), "\n"]),
	fail.

% =====================================================================
