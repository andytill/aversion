-module(mod_1_syntax).
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
		"hello",
		get_field1({myrec, 1, "hello"})
	).

get_field1_v2_test() ->
	?assertEqual(
		"hey",
		get_field1({myrec, 2, "hey", nnn})
	).

get_field1_v1_in_tuple_test() ->
	?assertEqual(
		{ok, "hello"},
		get_field1_in_tuple({myrec, 1, "hello"})
	).

get_field1_v2_in_tuple_test() ->
	?assertEqual(
		{ok, "hey"},
		get_field1_in_tuple({myrec, 2, "hey", nnn})
	).

get_field_v1_in_list_test() ->
	?assertEqual(
		[oko],
		get_field1_in_list({myrec, 1, oko})
	).

get_field_v1_in_list_as_tail_test() ->
	?assertEqual(
		[oko, banjo],
		get_field1_in_list_as_tail({myrec, 1, banjo})
	).

get_field_v1_in_case_expression_test() ->
	?assertEqual(
		a_banjo,
		a_case_expression({myrec, 1, banjo})
	).

unpack_field1_v1_in_function_clause_body_test() ->
	?assertEqual(
		oko,
		unpack_in_function_clause_body({myrec, 1, oko})
	).

%%%
%%%
%%%

unpack_in_function_clause_body(MyRec) ->
	#myrec{ field1 = Field } = MyRec,
	Field.

a_case_expression(#myrec{ field1 = F }) ->
	case F of
		oko ->
			an_oko;
		banjo ->
			a_banjo
	end.

get_field1(#myrec{ field1 = F }) ->
	F.
	
get_field1_in_tuple(#myrec{ field1 = F }) ->
	{ok, F}.

get_field1_in_list(#myrec{ field1 = F }) ->
	[F].

get_field1_in_list_as_tail(#myrec{ field1 = F }) ->
	[oko, F].
