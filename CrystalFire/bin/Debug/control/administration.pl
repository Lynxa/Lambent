% =====================================================================

:- ["../utils/utils"].

% =====================================================================

administration(CommunicationAdministrationHost, CommunicationAdministrationPort) :-

        socket(internet, stream, CommunicationAdministrationChannel),
        connect(CommunicationAdministrationChannel, CommunicationAdministrationHost/CommunicationAdministrationPort),
        show(["\n"]),
        show(["[Administration] ", "Established connection with <Communication Module>.\n"]),
        
        repeat,
                show(["\n"]),
                show(["[Administration] ", "Enter message for <Communication Module>: "]), read(Message),
                send(CommunicationAdministrationChannel, Message),
        Message == shutdown,
        close(CommunicationAdministrationChannel),
        show(["\n"]),
        show(["[Administration] ", "Shutting down...\n"]).

% =====================================================================
