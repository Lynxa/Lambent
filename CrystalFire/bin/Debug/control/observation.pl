% =====================================================================

:- ["../utils/utils"].


show_detail([

%        observation_incoming_message_notification,
        observation_incoming_message,

        any_unspecified_messages

]).

% =====================================================================

observation(CommunicationObservationHost, CommunicationObservationPort) :-

        socket(internet, stream, CommunicationObservationChannel),
        connect(CommunicationObservationChannel, CommunicationObservationHost/CommunicationObservationPort),
        show(["\n"]),
        show(["[Observation] ", "Established connection with <Communication Module>.\n"]),

        repeat,

              sleep(0.1), % To alleviate the problem of continuous loops.
              receive(CommunicationObservationChannel, InMessage),
              show(observation_incoming_message_notification, ["\n"]),
              show(observation_incoming_message_notification, ["[Observation] ", "Incoming message: (suppressed)\n"]),
              show(observation_incoming_message, ["\n"]),
              show(observation_incoming_message, ["[Observation] ", "Incoming message: ", InMessage, "\n"]),

        (InMessage == disconnected),
        close(CommunicationObservationChannel),
        show(["\n"]),
        show(["[Observation] ", "Shutting down...\n"]).

% =====================================================================
