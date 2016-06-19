-module(mod_2_syntax).
-compile([export_all]).
-compile({parse_transform, aversion}).

-include_lib("eunit/include/eunit.hrl").

-aversion([myrec]).

-record(myrec, {
	version = 1,
	field_a = a,
	field_b = b,
	field_c = c,
	field_d = d,
	field_e = e
}).

-record(myrec, {
	version = 2,
	field_f = f,
	field_g = g
}).

%%
%%

larger_records_test() ->
	?assertEqual(
		{a,b,c,d,e,f,g},
		myrec_unpack({myrec, 2, a,b,c,d,e,f,g})
	).

myrec_unpack(#myrec{ field_a = A, field_b = B, field_c = C, field_d = D,
	                 field_e = E, field_f = F, field_g = G }) ->
	{A,B,C,D,E,F,G}.

%%
%%


