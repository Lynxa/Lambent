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

	(orange, [
		owned_by,
		held_by,
		weight
	]),

	(pear, [
		owned_by,
		held_by,
		weight
	]),

	(combinatorial_auction, [
		owned_by,
		held_by,
		auctioneer,
		status,
		set_of_items,
		set_of_bids,
		allocation,
		prices,
		payments,
		marginal_allocations
		marginal_prices,
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


	issue(right(open_auction(Auction, SetOfItems, OpeningPrice), (
			(object(Item),
			value(Item, owned_by, Agent)) where member(Item, SetOfItems)
		)), Agent),

	transaction([
	        create_object(apple(Agent), apple),
	        create_object(orange(Agent), orange),
	       	create_object(pear(Agent), pear),

		set_properties(apple(Agent), [(weight, Weight)])
		where choose(Weight, [(0.2, 4), (0.3, 6), (0.5, 5)]),

		set_properties(orange(Agent), [(weight, Weight)])
		where choose(Weight, [(0.2, 4), (0.3, 6), (0.5, 5)]),

		set_properties(pear(Agent), [(weight, Weight)])
		where choose(Weight, [(0.2, 4), (0.3, 6), (0.5, 5)]),

	        give(apple(Agent), Agent) where choose(true, [(0.8, true), (0.2, false)],
	        give(orange(Agent), Agent) where choose(true, [(0.8, true), (0.2, false)],
	        give(pear(Agent), Agent) where choose(true, [(0.8, true), (0.2, false)]
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


action(Agent, create_auction(Auction, SetOfItems, OpeningPrices)) :-

	preconditions([
		\+ object(Auction)
	]),

	effects([
		create(Auction, combinatorial_auction),
		set(Auction, owned_by, Agent),
		set(Auction, held_by, Agent),
		set(Auction, auctioneer, Agent),
		set(Auction, status, open),
		set(Auction, set_of_items, SetOfItems),
		set(Auction, set_of_bids, OpeningPrices),
		set(Auction, allocation, undefined),
		set(Auction, prices, undefined),
		set(Auction, payments, undefined),
		set(Auction, marginal_allocations, undefined),
		set(Auction, marginal_prices, undefined),
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
					(set_of_bids, SetOfBids),
					(allocation, Allocation), % Only contains allocated items to bidders.
					(prices, Prices),
					(payments, Payments),
					(marginal_allocations, AllocationPerMarginalMarket),
					(marginal_prices, PricesPerMarginalMarket)
				]),
				AllAllocations = [Allocation|AllocationPerMarginalMarket],
				AllPrices = [Prices|PricesPerMarginalMarket],
				checkOutcomeEffciency(SetOfBids, AllAllocations, AllPrices),
				checkVCGPayments(SetOfBids, AllAllocations, Payments)
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
					(allocation, Allocation),
					(payments, Payments),
				]),
				(
					object(Event),
					value(Event, [
						(instance_of, event),
						(description, invoked(Agent, sell_bundle(WonItems, Payment, Bidder), successfully))
						% We ask that the sell_bundle action was succesful.
					])
				) where (member((Bidder,WonItems), Allocation), member((Bidder,Payment), Payments)),
			), (
				value(Auction, [
					(status, closed),       % This guarantees that the rest are meaningful.
				]),
				value(clock, happened_at, Time),
				atleast(Time, ClosingTime+100),
			), (
				jail(Agent)
			))),


		% Make the auction visible to other agents,
		% but not the set_of_bids property.
		% The competitive equilibrium prices and the
		% efficient allocations are viewable to the agents.
		issue_p(right(query(Auction, P), (
				value(Auction, status, open),
				P \= set_of_bids
			)), Bidder),
			% This is okay, since the issuer owns the right.
			% Otherwise you need to give individual rights.

		% Make the item visible to other agents.
		issue_p(right(query(Item, _), (
				value(Auction, [(status, open), (set_of_items, SetOfItems)]),
				member(Item, SetOfItems)
			)), Bidder),
			% This is okay, since the issuer owns the right.
			% Otherwise you need to give individual rights.

		issue_p(right(place_bid(Auction, Bid, Bundle), (
				value(Auction, status, open)
			)), Bidder)
			% This is okay, since the issuer owns the right.
			% Otherwise you need to give individual rights.

	]).

% =====================================================================


action(Agent, submit_bid(Auction, Bid, Bundle)) :-

	preconditions([
		object(Auction)
		value(Auction, set_of_bids, SetOfItems),
		includes(SetOfItems, Bundle)
		]),

	effects([
		set(Auction, set_of_bids, [(Agent,Bid,Bundle)|SetOfBids) where value(Auction, set_of_bids, SetOfBids),
		set(Auction, last_bid_time, Time) where value(clock, happened_at, Time)
	]).

% =====================================================================

action(Agent, place_bid(Auction, Bid, Bundle)) :-

	transaction([
		submit_bid(Auction, Bid, Bundle),
		issue_p(right(sell_bundle(Bundle, Payment, Agent), (
			value(Auction, [(status, closed),
					(allocation, Allocation),
					(payments, Payments)
				]),
			member((Bidder,Bundle), Allocation),
			member((Bidder,Payment), Payments)),
			value(clock, happened_at, Time),
			atleast(ClosingTime+100, Time)
		)), Auctioneer) where value(Auction, [
				(auctioneer, Auctioneer)])
	]).


% =====================================================================

action(Agent, close_auction(Auction, Allocation, Prices, Payments, AllocationPerMarginalMarket, PricesPerMarginalMarket)) :-

	preconditions([
		object(Auction)
	]),

	effects([
		set(Auction, status, closed),
		set(Auction, allocation, Allocation), % Only contains allocated items to bidders.
		set(Auction, prices, Prices),
		set(Auction, payments, Payments),
		set(Auction, (marginal_allocations, AllocationPerMarginalMarket),
		set(Auction, (marginal_prices, PricesPerMarginalMarket),
		set(Auction, closing_time, Time) where value(clock, happened_at, Time)
	]).

% =====================================================================

action(Agent, sell_bundle(Items, Price, SomeAgent)) :-

	transaction([
		give(Item, SomeAgent) where member(Item, Items),
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
