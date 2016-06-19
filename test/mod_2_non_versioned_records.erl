-module(mod_2_non_versioned_records).

%%%
%%% Test that versioned records and non-version can interact.
%%%

-compile([export_all]).
-compile({parse_transform, aversion}).

-include_lib("eunit/include/eunit.hrl").

-aversion([vrec]).

-record(vrec, {
	version = 1,
	field1 = 1
}).

-record(vrec, {
	version = 2,
	field2
}).

%% this record is not versioned
-record(other_rec, { field1 }).

other_rec_can_be_created_test() ->
	?assertEqual(
		{other_rec, value},
		#other_rec{ field1 = value }
	).

%%
%%

other_rec_can_be_put_in_function_headers_test() ->
	?assertEqual(
		hola,
		do_function_header(#other_rec{ field1 = hola })
	).

do_function_header(#other_rec{ field1 = V }) ->
	V.

%%
%%