% =====================================================================

:- [tools].

% =====================================================================

user(ProxyHost, ProxyPort) :-

	socket(internet, stream, ProxySocket),
	connect(ProxySocket, ProxyHost/ProxyPort),
	show(["\n"]),
	show(["[User] ", "Connected to proxy.\n"]),

	repeat,

		show(["\n"]),
		show(["[User] ", "Enter action to invoke: "]),
		read(Action), send(ProxySocket, Action),

	Action == leave,
      	close(ProxySocket),
	show(["\n"]),
	show(["[User] ", "Disconnected from proxy.\n"]).

% =====================================================================
