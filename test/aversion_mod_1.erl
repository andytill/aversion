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

do_the_thing(#myrec{ field1 = F }) ->
	{hi, F}.