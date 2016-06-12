-module(aversion).

-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

-type erlang_code() :: string().
-type ast_record() :: term().
%%-type ast_record_field() :: term().

parse_transform(AST_in, _Options) ->
	%% debug
    io:format("== AST ==~n~p~n== AST ==~n~n", [AST_in]),
    %% find which records need to be versioned
    Aversion_records = lists:foldl(fun fold_aversion_attributes/2, [], AST_in),
    %% find the definitions of those versioned records
    Aversion_defs =
    	lists:foldl(fun fold_records/2, new_defs_dict(Aversion_records), AST_in),
	First_field_index = 3,  
    Function_list =
    	dict:fold(
	    	fun(_K, V, Acc) ->
	    		build_record_unpack_field_functions(First_field_index, V, Acc)
	    	end, [], Aversion_defs),
    io:format("FUNCTION LIST~n~p~n", [Function_list]),
	%% TODO generate functions that get record fields
	%% TODO generate functions that write record fields
	%% TODO iterate through the AST and convert record unpacking and creation
	%%		into calls to the generated functions

    %% remove the records that appear in -aversion attribute, they can't be kept
    %% as records because there will be many versions with the same name.
    AST_out =
    	[AST_x || AST_x <- AST_in, not is_aversion_record(Aversion_defs, AST_x)],
	%% debug
    % io:format("~n== New AST ==~n~p~n== New AST ==~n~n", [AST_out]),
    AST_out.

%% Build a dict of field atom to a list of function heads as erlang code as a
%% string.
-spec build_record_unpack_field_functions(N::integer(),
										  Records::[ast_record()],
										  Function_list::[erlang_code()]) -> [erlang_code()].
build_record_unpack_field_functions(_, [], Function_list) ->
	Function_list;
build_record_unpack_field_functions(N, [Record|Tail], Function_list_1) ->
	Record_name = ast_record_name(Record),
	Fields = ast_record_fields(Record),
	Fn =
		fun(Field_x,Acc) ->
			[build_unpacker_function(Record_name, N, Field_x) | Acc]
		end,
	Function_list_2 = lists:foldl(Fn, [], Fields),
	build_record_unpack_field_functions(N+1, Tail, Function_list_1 ++ Function_list_2).

%%
build_unpacker_function(Record_name, N, Field) ->
	Field_name = ast_record_field_name(Field),
	Fn_name = field_name_getter_function_name(Record_name, Field_name),
	Fn_name ++ "(X) when element(1,X) == " ++ atom_to_list(Record_name) ++ " -> element(" ++ integer_to_list(N) ++ ",X).".

%%
-spec field_name_getter_function_name(atom(), atom()) -> string().
field_name_getter_function_name(Record_name, Field_name) when is_atom(Field_name) ->
	"aversion_" ++ atom_to_list(Record_name) ++ "_" ++ atom_to_list(Field_name).

%%
is_aversion_record(Defs_dict, AST_element) ->
	is_record(AST_element) andalso dict:is_key(ast_record_name(AST_element), Defs_dict).

%% accumulate all of the records named in the aversion attribute
fold_aversion_attributes({attribute,_,aversion,Record_list}, Acc) ->
	Acc ++ Record_list;
fold_aversion_attributes(_, Acc) ->
	Acc.

%% build a dict with all of the defined versioned record names. The dict is
%% keyed on the record atom name, the value is a list of record ASTs.
new_defs_dict(Record_list) ->
	dict:from_list([{N, []} || N <- Record_list]).

%%
fold_records(AST_element, Defs_dict) ->
	case is_aversion_record(Defs_dict, AST_element) of
		true ->
			Rec_name = ast_record_name(AST_element),
			Def_list = dict:fetch(Rec_name, Defs_dict),
			ok = validate_versioned_record(AST_element, Def_list),
			dict:append_list(Rec_name, [AST_element], Defs_dict);
		false ->
			Defs_dict
	end.

%% the record must have a version field with an integer
%% TODO the integer version must be an increment of the ast version
%% TODO    or 1 if there are no previous versions
validate_versioned_record(Record, _Defs) ->
	Field = ast_record_field(1, Record),
	case ast_record_field_name(Field) of
		version ->
			ok;
		Other_name ->
			error({first_field_must_be_version,Other_name})
	end,
	case ast_record_field_value(Field) of
		N when is_integer(N) ->
			ok;
		V ->
			error({version_must_be_integer,V})
	end.

%%% ============================================================================
%%% AST functions
%%% ============================================================================

%%
is_record(AST) ->
	(size(AST) >= 3 andalso element(3,AST) == record).

%%
ast_record_name({_,_,record,{Rec_name,_}}) ->
	Rec_name.

ast_record_field(N, {_,_,record,{_,Fields}}) when is_integer(N) ->
	lists:nth(N, Fields).

%%
ast_record_fields({_,_,record,{_,Fields}}) ->
	Fields.

%%
ast_record_field_name(Field) when element(1,Field) == record_field ->
	{atom,_,Field_name} = element(3,Field),
	Field_name.

%%
ast_record_field_value(Field) when element(1,Field) == record_field ->
	case size(Field) of
		4 ->
			{_Type,_,Value} = element(4,Field),
			Value;
		_ ->
			error({field_has_no_default, Field})
	end.
