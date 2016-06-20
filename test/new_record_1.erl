-module(new_record_1).
-compile([export_all]).
-compile({parse_transform, aversion}).

-include_lib("eunit/include/eunit.hrl").

-aversion([myrec]).

-record(myrec, {
	version = 1,
	field1 = banjo
}).

-record(myrec, {
	version = 2,
	field2 = oko
}).

%%
%%

new_default_record_test() ->
	?assertEqual(
		{myrec,2,banjo,oko},
		default_myrec()	
	).

default_myrec() -> #myrec{ }.

%%
%%

new_record_with_field1_set_test() ->
	?assertEqual(
		{myrec,2,dingo,oko},
		default_myrec_with_field1_set()	
	).

default_myrec_with_field1_set()	-> #myrec{ field1 = dingo }.

%%
%%
