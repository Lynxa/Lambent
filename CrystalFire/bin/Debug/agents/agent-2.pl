% =====================================================================

%:- use_module(library(eplex)).
:- [client].
:- dynamic state/1, name/1, beliefs/1.

% =====================================================================

initial_beliefs([(money, 195), (value(apple), 15), (value(car), 4500)]).

% =====================================================================

age :- agent('LOIZOS-PC', 5400).

agent(ServerHost, ServerPort) :-
        assert(state([])), assert(name(unknown)), randomise,
        initial_beliefs(Beliefs), assert(beliefs(Beliefs)), 
        show(["\n"]),
        show(["[Agent] ", "Hello! My name is Bob Nuts. I believe that in a previous life I was the king of France.\n"]),
        show(["\n"]),
        show(["[Agent] ", "I have nothing to do. What if I join this simulation?\n"]),
        client(ServerHost, ServerPort),
        show(["\n"]),
        show(["[Agent] ", "I am bored with this simulation. I am leaving...\n"]).

% =====================================================================

process_message(msg(connected(Name), _)) :-
        !, retract(name(_)), assert(name(Name)),
        show(["\n"]),
        show(["[Agent] ", "Hahaha! For some reason the simulation refers to me by the number: ", Name, "\n"]).

process_message(msg(state(State), _)) :-
        !, retract(state(_)), assert(state(State)),
        show(["\n"]),
        show(["[Agent] ", "Oh! A state update message. As if I care.\n"]),
        view_state(State).

process_message(msg(state_change(Deleted, Added), _)) :-
        !, retract(state(OldState)),
        subtract(OldState, Deleted, Unchanged),
        union(Unchanged, Added, NewState),
        assert(state(NewState)),
        show(["\n"]),
        show(["[Agent] ", "Oh! A state update message. As if I care.\n"]),
        view_state(NewState).

process_message(msg(state_update(DeletedObjects, DeletedProperties, UpdatedProperties), _)) :-
        !, retract(state(OldState)),
        state_reconstruct(OldState, DeletedObjects, DeletedProperties, UpdatedProperties, NewState),
        assert(state(NewState)),
        show(["\n"]),
        show(["[Agent] ", "Oh! A state update message. As if I care.\n"]),
        view_state(NewState).

process_message(msg(disconnected, _)) :-
        !, retract(name(_)), assert(name(disconnected)),
        retract(state(_)), assert(state([])),
        show(["\n"]),
        show(["[Agent] ", "Grrr... Someone disconnected me from the simulation.\n"]).

process_message(_) :-
        show(["\n"]),
        show(["[Agent] ", "Another kind of message?!? Wow, this simulation is really cool!\n"]).

% =====================================================================

take_decision(disconnected) :-
        retract(name(disconnected)), assert(name(unknown)).

take_decision(Action) :-
        choose(Action, [
                (0.00000001, fly),
                (0.00000001, eat_ice_cream),
                (0.00000001, sing("99 bottles of beer on the wall"))
        ]),
say(Action),
        show(["\n"]),
        (Action = fly ->
                show(["[Agent] ", "I am a butterfly and I want to fly.\n"])
        ;
        (Action = eat_ice_cream ->
                show(["[Agent] ", "I wonder if there is any ice-cream left from last Thursday.\n"])
        ;
        (Action = sing("99 bottles of beer on the wall") ->
                show(["[Agent] ", "I feel like singing.\n"])
        ;
        false
        ))).

% =====================================================================
