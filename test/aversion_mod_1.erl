-module(aversion_mod_1).
-compile([export_all]).
-compile({parse_transform, aversion}).

-include_lib("eunit/include/eunit.hrl").

-aversion([myrec]).

-record(myrec, {
	version = 1,
	field1 = 1
}).

-record(myrec, {
	version = 2,
	field2
}).

get_field1_v1_test() ->
	?assertEqual(
		{ok, "hello"},
		get_field1({myrec, 1, "hello"})
	).

get_field1_v2_test() ->
	?assertEqual(
		{ok, "hello"},
		get_field1({myrec, 2, "hello", nnn})
	).


get_field1(#myrec{ field1 = F }) ->
	{ok, F}.

% do_the_thing_match(#myrec{ field1 = F } = _XXXX) ->
% 	{hi, F}.

do_the_thing_rewrite(MyRec) when element(1,MyRec) ->
	{hi, element(3,MyRec)}.
