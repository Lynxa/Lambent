% =========================================================================================================================
%	 						Simulation Submodule: Database Management
% =========================================================================================================================

% Defines the DB interface.

% Reserves the following predicates:

% Reserves the following stream names:
% db_stream_read, db_stream_write.

% Accesses the file:
% history.db


% =====================================================================
% Database Management Predicates
% =====================================================================

db_clear :-
	open("history.db", write, db_stream_write),
	close(db_stream_write).

db_open :-
	open("history.db", append, db_stream_write),
	open("history.db", read, db_stream_read).

db_store(Data) :-
	writeq(db_stream_write, Data),
	write(db_stream_write, '. \n\n'),
	flush(db_stream_write).

% Assumes file has be opened.
% Returns next piece of data during backtrack.
% Closes file when EOF reached, and fails.
db_retrieve(Data) :-
	repeat,
		read(db_stream_read, Data),
	(Data == end_of_file ->
		(db_close, !, fail)
	; true).

db_close :-
	close(db_stream_read),
	close(db_stream_write).

% =====================================================================

