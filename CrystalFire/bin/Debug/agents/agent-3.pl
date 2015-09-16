% =====================================================================

%:- use_module(library(eplex)).
:- [client].
:- dynamic state/1, name/1, beliefs/1, ignore/1.

% =====================================================================

% Saves the agent from performing futile actions.
ignore(dummy).

% =====================================================================

initial_beliefs([
        (money, 200),
        (value(apple, [(weight, 3)]), 5),
        (value(apple, [(weight, 4)]), 8),
        (value(apple, [(weight, 5)]), 8),
        (value(apple, [(weight, 6)]), 10),
        (value(car), 6000)
]).

% =====================================================================

age :- agent('LOIZOS-PC', 5400).

agent(ServerHost, ServerPort) :-
        assert(state([])), assert(name(unknown)), randomise,
        initial_beliefs(Beliefs), assert(beliefs(Beliefs)),
        show(["\n"]),
        show(["[Agent] ", "Hello! My name is Carl Wise. These are my initial beliefs: ", Beliefs, "\n"]),
        show(["\n"]),
        show(["[Agent] ", "This simulation looks interesting. I think I'll join.\n"]),
        client(ServerHost, ServerPort),
        show(["\n"]),
        show(["[Agent] ", "I am done using this simulation. I am leaving...\n"]).

% =====================================================================

process_message(msg(connected(Name), _)) :-
        !, retract(name(_)), assert(name(Name)),
        show(["\n"]),
        show(["[Agent] ", "I am connected to the simulation under the name: ", Name, "\n"]).

process_message(msg(state(State), _)) :-
        !, retract(state(_)), assert(state(State)),
        show(["\n"]),
        show(["[Agent] ", "I'd better update my local copy of the simulation state.\n"]),
        view_state(State).

process_message(msg(state_change(Deleted, Added), _)) :-
        !, retract(state(OldState)),
        subtract(OldState, Deleted, Unchanged),
        union(Unchanged, Added, NewState),
        assert(state(NewState)),
        show(["\n"]),
        show(["[Agent] ", "I'd better update my local copy of the simulation state.\n"]),
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
        show(["[Agent] ", "I have been disconnected from the simulation!\n"]).

process_message(_) :-
        show(["\n"]),
        show(["[Agent] ", "What is this?!? I cannot understand this message.\n"]).

% =====================================================================

take_decision(disconnected) :-
        retract(name(disconnected)), assert(name(unknown)).

take_decision(place_bid(Auction, Bid)) :-
        clause(state(State)), clause(name(Name)), clause(beliefs(Beliefs)),

        objectin(Auction, State),
        valuein(Auction, [(instance_of, english_auction), (status, open), (item, X), (highest_bid, Y), (highest_bidder, N)], State),
        N \= Name, Bid is Y+1,
        \+ clause(ignore(place_bid(Auction, Bid))),
        valuein(X, [(instance_of, C)], State),
        show(["\n"]),
        show(["[Agent] ", "Oh, look! A(n) ", C, " is being auctioned...\n"]),
        show(["[Agent] ", "The current highest bid is $", Y," ...\n"]),

        member((value(C, Properties), Z), Beliefs),
        valuein(X, Properties, State),
        ((Z >= Bid) -> (
                show(["[Agent] ", "... and I value this item for $", Z, ".\n"])
        ) ; (
                show(["[Agent] ", "... but I only value this item for $", Z, ".\n"]),
                assert(ignore(place_bid(Auction, _))),
                fail
        )),

        objectin(account(Name), State),
        valuein(account(Name), [(owned_by, Name), (amount, M)], State),
        ((M >= Bid) -> (
                show(["[Agent] ", "Luckily, I have $", M, " in my account and I can raise the bid.\n"])
        ) ; (
                show(["[Agent] ", "Unfortunately, I only have $", M, " in my account and I cannot raise the bid.\n"]),
                assert(ignore(place_bid(Auction, _))),
                fail
        )),

        (choose(_, [(0.9, _)]) -> (
                show(["[Agent] ", "Hmmm... I think I will place a bid for $", Bid, ".\n"]),
                assert(ignore(place_bid(Auction, Bid)))
        ) ; (
                show(["[Agent] ", "I don't feel like placing a bid though.\n"]),
                fail
        )).

% Only works for apple auctions.
take_decision(leave) :-
        clause(state(State)), clause(name(Name)),

        objectin(apple(N), State), N \=Name,
        valuein(apple(N), owned_by, Name, State),
        show(["\n"]),
        show(["[Agent] ", "Yeah! I won the auction! I'd better leave the simulation before anyone comes looking for this apple...\n"]).

% =====================================================================
