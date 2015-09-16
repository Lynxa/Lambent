% =========================================================================================================================
% 									Domain Specification
% =========================================================================================================================




% =====================================================================
% Class Specification
% =====================================================================


classes([

	(apple, [
		owned_by,
		held_by,
		weight
	]),

	(sealed_auction, [
		owned_by,
		held_by,
		auctioneer,
		status,
		item,
		set_of_bids,
		winner,
		payment,
		last_bid_time,
		closing_time
	])

]).


% =====================================================================
% Initial Specification
% =====================================================================


initially([]).


% =====================================================================
% On Entrance Specification
% =====================================================================

on_entrance(Agent, [

	issue(right(open_auction(Auction, Item, OpeningPrice), (
			object(Item),
			value(Item, owned_by, Agent)
		)), Agent),

	transaction([
	        create_object(apple(Agent), apple),

		set_properties(apple(Agent), [(weight, Weight)])
		where choose(Weight, [(0.2, 4), (0.3, 6), (0.5, 5)]),

	        give(apple(Agent), Agent)
	]),

        set_properties(account(Agent), [(amount, Amount)])
	where choose(Amount, [(0.1, 10), (0.2, 4), (0.2, 6), (0.4, 7), (0.1, 1)])

]).


% =====================================================================
% On Departure Specification
% =====================================================================

on_departure(Agent, []).


% =====================================================================
% Action Specification
% =====================================================================

action(Agent, create_auction(Auction, Item, OpeningPrice)) :-

	preconditions([
		\+ object(Auction)
	]),

	effects([
		create(Auction, sealed_auction),
		set(Auction, owned_by, Agent),
		set(Auction, held_by, Agent),
		set(Auction, auctioneer, Agent),
		set(Auction, status, open),
		set(Auction, item, Item),
		set(Auction, set_of_bids, [(Agent,OpeningPrice)]),
		set(Auction, winner, undefined),
		set(Auction, payment, undefined),
		set(Auction, last_bid_time, Time) where value(clock, happened_at, Time),
		set(Auction, closing_time, undefined)
	]).

% =====================================================================

action(Agent, open_auction(Auction, Item, OpeningPrice)) :-

	transaction([

		create_auction(Auction, Item, OpeningPrice),

		take_on(obligation(
			(
				value(Auction, [
					(status, closed),
					(set_of_bids, SetOfBids)
					(winner, HighestBidder),
					(payment, SecondHighestBid)
				]),
				get_second_price(SetOfBids, SecondHighestBid),
				get_first_bidder(SetOfBids, HighestBidder)
			), (
				value(clock, happened_at, Time),
				value(Auction, last_bid_time, LastBidTime),
				atleast(Time, LastBidTime+100)
			), (
				jail(Agent)
			))),

		take_on(obligation(
			(
				value(Auction, [
					(status, closed),
					(winner, HighestBidder),
					(payment, SecondHighestBid)
				]),
				object(Event),
				value(Event, [
					(instance_of, event),
					(description, invoked(Agent, sell(Item, SecondHighestBid, HighestBidder), successfully))
					% We ask that the sell action was succesful.
				])

			), (
				value(Auction, [
					(status, closed),       % This guarantees that the rest are meaningful.
					(winner, HighestBidder),
				]),
				value(clock, happened_at, Time),
				atleast(Time, ClosingTime+100),
				HighestBidder \= Agent		% The obligation only holds if there was a bid.
			), (
				jail(Agent)
			))),


		% Make the auction visible to other agents,
		% but not the set_of_bids property.
		issue_p(right(query(Auction, P), (
				value(Auction, status, open),
				P \= set_of_bids
			)), Bidder),
			% This is okay, since the issuer owns the right.
			% Otherwise you need to give individual rights.

		% Make the item visible to other agents.
		issue_p(right(query(Item, _), (
				value(Auction, status, open)
			)), Bidder),
			% This is okay, since the issuer owns the right.
			% Otherwise you need to give individual rights.

		issue_p(right(place_bid(Auction, Bid), (
				value(Auction, status, open)
			)), Bidder)
			% This is okay, since the issuer owns the right.
			% Otherwise you need to give individual rights.

	]).

% =====================================================================


action(Agent, submit_bid(Auction, Bid)) :-

	preconditions([
		object(Auction)
		value(Auction, set_of_bids, SetOfBids),
		\+ member((Agent,_), SetOfBids)
		]),

	effects([
		set(Auction, set_of_bids, [(Agent,Bid)|SetOfBids) where value(Auction, set_of_bids, SetOfBids),
		set(Auction, last_bid_time, Time) where value(clock, happened_at, Time)
	]).

% =====================================================================

action(Agent, place_bid(Auction, Bid)) :-

	transaction([
		submit_bid(Auction, Bid),
		issue_p(right(sell(Item, Bid, Agent), (
			value(Auction, [(status, closed),
					(payment, Bid),
					(winner, Agent)]),
			value(clock, happened_at, Time),
			atleast(ClosingTime+100, Time)
		)), Auctioneer) where value(Auction, [
				(auctioneer, Auctioneer),
				(item, Item)])
	]).


% =====================================================================

action(Agent, close_auction(Auction, Winner, Outcome)) :-

	preconditions([
		object(Auction)
	]),

	effects([
		set(Auction, status, closed),
		set(Auction, winner, Winner),
		set(Auction, payment, Payment),
		set(Auction, closing_time, Time) where value(clock, happened_at, Time)
	]).

% =====================================================================

action(Agent, sell(Item, Price, SomeAgent)) :-

	transaction([
		give(Item, SomeAgent),
		take_money(Price, SomeAgent)
	]).


% =====================================================================
% Predicate Specification
% =====================================================================

delay atleast(A,B) if (nonground(A) ; nonground(B)).
atleast(A,B) :-
	eval(A, EA), eval(B, EB), EA >= EB.

% =====================================================================
% This is the end of the domain description. Do not remove this line! #
% =====================================================================
