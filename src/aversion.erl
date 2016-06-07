-module(aversion).

-export([parse_transform/2]).

parse_transform(AST_in, _Options) ->
	%% debug
    io:format("== AST ==~n~p~n== AST ==~n~n", [AST_in]),
    %% find which records need to be versioned
    Aversion_records = lists:foldl(fun fold_aversion_attributes/2, [], AST_in),
    %% find the definitions of those versioned records
    Aversion_defs =
    	lists:foldl(fun fold_records/2, new_defs_dict(Aversion_records), AST_in),
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
		{_,_,version} ->
			ok;
		{_,_,Other_name} ->
			error({first_field_must_be_version,Other_name})
	end,
	case ast_record_field_value(Field) of
		{integer,_,N} when is_integer(N) ->
			ok;
		{integer,_,V} ->
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
ast_record_field_name(Field) ->
	element(3,Field).

%%
ast_record_field_value(Field) ->
	element(4,Field).
