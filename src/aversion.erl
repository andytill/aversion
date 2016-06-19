%%% ============================================================================
%%% erlang parse transform for versioned records
%%%
%%% Copyright (C) 2016  Andy Till kittyburrito@gmail.com
%%% 
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%% 
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%% 
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% ============================================================================

-module(aversion).

-export([parse_transform/2]).

-type erlang_code() :: string().
-type ast()         :: term().
-type ast_record()  :: term().
-type ast_record_attribute()  :: term().

-record(clause,   { line_num :: integer(),
	                args     :: list(),
	                guards   :: list(),
	                body     :: list()
}).
-record(function, { line_num :: integer(),
	                fn_name  :: atom(),
	                arity    :: integer(),
	                clauses  :: [#clause{}]
}).

parse_transform(AST_in, _Options) ->
	%% debug
    %%io:format("== AST ==~n~p~n== AST ==~n~n", [AST_in]),
    %% find which records need to be versioned
    Aversion_records = lists:foldl(fun fold_aversion_attributes/2, [], AST_in),
    %% find the definitions of those versioned records
    Aversion_defs =
    	lists:foldl(fun fold_records/2, new_defs_dict(Aversion_records), AST_in),
	First_field_index = 2,  
    Function_list =
    	dict:fold(
	    	fun(_K, V, Acc) ->
	    		build_record_unpack_field_functions(First_field_index, V, Acc)
	    	end, [], Aversion_defs),
    %% create a dict of the record names, tagged with `aversion_record` for
    %% namespacing, since this dict will also contain the variable names for fields
    Record_names = [{{aversion_record, R}, ok} || R <- dict:fetch_keys(Aversion_defs)],
	AST_out_1 = walk_ast(AST_in, [], dict:from_list(Record_names)),
    AST_out_2 = filter_aversion_records(Aversion_defs, AST_out_1),
    AST_out_3 = insert_function_asts([merl:quote(F) || {_,F} <- Function_list], AST_out_2),
    %%
    New_record_function_list =
    	dict:fold(
	    	fun(K, V, Acc) ->
	    		[build_new_record_fns(K, V, 1, []) | Acc]
	    	end, [], Aversion_defs),

    AST_out_4 = insert_function_asts(New_record_function_list, AST_out_3),
	%% debug
    %% io:format("~n== AST OUT ==~n~p~n== AST OUT ==~n~n", [AST_out_3]),
    AST_out_4.

%% remove the records that appear in -aversion attribute, they can't be kept
%% as records because there will be many versions with the same name.
filter_aversion_records(Aversion_defs, AST) ->
	[AST_x || AST_x <- AST, not is_aversion_record(Aversion_defs, AST_x)].

%% Build a dict of field atom to a list of function heads as erlang code as a
%% string.
-spec build_record_unpack_field_functions(N::integer(),
										  Records::[ast_record_attribute()],
										  Function_list::[erlang_code()]) -> [erlang_code()].
build_record_unpack_field_functions(_, [], Function_list) ->
	lists:reverse(Function_list);
build_record_unpack_field_functions(N_1, [Record|Tail], Function_list_1) ->
	Record_name = ast_record_attribute_name(Record),
	Fields = ast_record_attribute_fields(Record),
	Fn =
		fun(Field_x, {N_x, Acc}) ->
			Field_name = ast_record_field_name(Field_x),
			Fn_key = {Record_name, Field_name},
			case lists:keymember(Fn_key, 1, Function_list_1) of
				false ->
					Fn_erlang = build_unpacker_function(Record_name, N_x, Field_name),
					{N_x+1,[{Fn_key, Fn_erlang} | Acc]};
				true ->
					{N_x, Acc}
			end
		end,
	{N_2, Function_list_2} = lists:foldl(Fn, {N_1, []}, Fields),
	build_record_unpack_field_functions(N_2, Tail, Function_list_1 ++ Function_list_2).

%%
build_unpacker_function(Record_name, N, Field_name) ->
	Fn_name = field_name_getter_function_name(Record_name, Field_name),
	Fn_name ++ "(X) when element(1,X) == " ++ atom_to_list(Record_name) ++ " -> element(" ++ integer_to_list(N) ++ ",X).".

%%
-spec field_name_getter_function_name(atom(), atom()) -> string().
field_name_getter_function_name(Record_name, Field_name) when is_atom(Field_name) ->
	"aversion_" ++ atom_to_list(Record_name) ++ "_" ++ atom_to_list(Field_name).

%%
is_aversion_record(Defs_dict, AST_element) ->
	is_record_attribute(AST_element) andalso dict:is_key(ast_record_attribute_name(AST_element), Defs_dict).

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
			Rec_name = ast_record_attribute_name(AST_element),
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
	Field = ast_record_attribute_field(1, Record),
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
%%% AST getters
%%% ============================================================================

%%
is_record_attribute(AST) ->
	(size(AST) >= 3 andalso element(3,AST) == record).

%%
ast_record_attribute_name({attribute,_,record,{Rec_name,_}}) ->
	Rec_name.

%%
ast_record_attribute_field(N, {attribute,_,record,{_,Fields}}) when is_integer(N) ->
	lists:nth(N, Fields).

%% Assume that the version field is always, and has been validated by this point.
ast_record_attribute_version({attribute,_,record,{_,[Version_field|_]}}) ->
	ast_record_field_value(Version_field).

%%
ast_record_attribute_fields({attribute,_,record,{_,Fields}}) ->
	Fields.

%%
ast_record_name({record,_,Record_name,_}) ->
	Record_name.

%%
ast_record_fields({record,_,_,Fields}) ->
	Fields.

%%
ast_record_field_name(Field) when element(1,Field) == record_field ->
	{atom,_,Field_name} = element(3,Field),
	Field_name.

%% Return the name of a field that has been unpacked...
%%     myrec{ field1 = My_var }
%% This will reuturn My_var as an atom().
ast_record_field_var({record_field,_,_,{var,_,Var_name}}) ->
	Var_name.

%%
ast_record_field_value(Field) when element(1,Field) == record_field ->
	case size(Field) of
		4 ->
			{_Type,_,Value} = element(4,Field),
			Value;
		_ ->
			error({field_has_no_default, Field})
	end.

%%
ast_record_field_value(Field, Default) when element(1,Field) == record_field ->
	case size(Field) of
		4 ->
			{_Type,_,Value} = element(4,Field),
			Value;
		_ ->
			Default
	end.

%%
ast_var_name({var,_,Var_name}) ->
	Var_name.

%% Creates a var, which is a variable name in erlang AST
new_ast_var(Ln_num, Var_name) when is_integer(Ln_num), is_atom(Var_name) ->
	{var, Ln_num, Var_name}.


%%% ============================================================================
%%% AST iteration
%%% ============================================================================

-define(is_dict(Dict), element(1,Dict) == dict).

%% The plan for unpacking record fields...
%%
%% Any time we encounter a `record_field` tuple, hold the record, field where
%% it came from and the var. Replace the record tuple with a plain var.  When
%% we encounter the var, switch it for a call to the unpacker function.
walk_ast([], AST_out, _) ->
	lists:reverse(AST_out);
walk_ast(AST_in, AST_out, Acc) when not is_list(AST_in), ?is_dict(Acc) ->
	[X] = walk_ast([AST_in], AST_out, Acc),
	X;
walk_ast([AST_in | Tail], AST_out_1, Acc_1) when ?is_dict(Acc_1) ->
	{AST_result, Acc_2} =
		case element(1,AST_in) of
			function ->
				{walk_function_ast(AST_in, Acc_1), Acc_1};
			var ->
				{walk_var_ast(AST_in, Acc_1), Acc_1};
			tuple ->
				{walk_tuple_ast(AST_in, Acc_1), Acc_1};
			'case' ->
				{walk_case_ast(AST_in, Acc_1), Acc_1};
			match ->
				walk_match_ast(AST_in, Acc_1);
			cons ->
				%% a list
				{walk_cons_ast(AST_in, Acc_1), Acc_1};
			nil ->
				%% end of a list
				{AST_in, Acc_1};
			record ->
				{walk_record_ast(AST_in, Acc_1), Acc_1};
			_ ->
				{AST_in, Acc_1}
		end,
	AST_out_2 =
		case AST_result of
			ignore_ast ->
				AST_out_1;
			_ ->
				[AST_result|AST_out_1]
		end,
	walk_ast(Tail, AST_out_2, Acc_2).

%%
walk_function_ast(#function{ clauses = Clauses_1 } = F, Acc) ->
	Clauses_2 = [walk_function_clause_ast(C, Acc) || C <- Clauses_1],
	F#function{ clauses = Clauses_2 }.

%% A clause is one function head within a function.
walk_function_clause_ast(#clause{ args = Args_1, body = Body_1 } = C, Acc_1) ->
	%% TODO this should return extra guards as well to make sure any vars are
	%%      records
	{Args_2, Acc_2} = walk_function_args_ast(Args_1, [], Acc_1),
	Body_2 = walk_ast(Body_1, [], Acc_2),
	C#clause{ args = Args_2, body = Body_2 }.

%%
-spec walk_function_args_ast([ast()], [ast()], Acc::term()) ->
		{AST_out::[ast()], Acc2::term()}.
walk_function_args_ast([], AST_out, Acc) ->
	{lists:reverse(AST_out), Acc};
walk_function_args_ast([Rec|Tail], AST_out, Acc_1) when element(1,Rec) == record ->
	%% check if this record is actually versioned, if not then do nothing
	case dict:is_key({aversion_record, ast_record_name(Rec)}, Acc_1) of
		true ->
			Record_var_name = new_record_var_name(Rec),
			{AST_result, Acc_2} = walk_record_unpack_ast(Rec, Record_var_name, Acc_1);
		false ->
			AST_result = Rec,
			Acc_2 = Acc_1
	end ,
	walk_function_args_ast(Tail, [AST_result|AST_out], Acc_2);
walk_function_args_ast([Other|Tail], AST_out, Acc) ->
	walk_function_args_ast(Tail, [Other|AST_out], Acc).

%%
-spec walk_record_unpack_ast(ast(), atom(), term()) -> {ast(), term()}.
walk_record_unpack_ast(Rec, Record_var_name, Acc_1) when is_atom(Record_var_name) ->
	%% does rec already have a defined name
	%%     if so use other
	%%     if not create one
	%% go through the fields and key the name it was unpacked to, against the
	%% record and field name.
	%% replace the record with variable name
	%% in the body replace uses of the field name with calls to the getter using
	%% ..the record and field name we have keyed against it.
	Fields = ast_record_fields(Rec),
	Record_name = ast_record_name(Rec),
	Ln_num = element(2,Rec),
	Record_var = new_ast_var(Ln_num, Record_var_name),
	Acc_2 = 
		lists:foldl(
			fun(Field, Acc_x) ->
				Field_name = ast_record_field_name(Field),
				Var_name = ast_record_field_var(Field),
				dict:store(Var_name, {Record_name,Field_name,Record_var_name}, Acc_x)
			end, Acc_1, Fields),
	{Record_var, Acc_2}.

walk_record_ast(Rec_ast, _Acc_1) ->
	Rec_name = ast_record_name(Rec_ast),
	merl:quote("aversion_new_" ++ atom_to_list(Rec_name) ++ "_latest()").

%%
walk_match_ast({match, _, Left, Right}, Acc_1) when element(1,Left) == record, element(1,Right) == var ->
	Var_name = ast_var_name(Right),
	{_, Acc_2} = walk_record_unpack_ast(Left, Var_name, Acc_1),
	%% unpacking a var to a record
	{ignore_ast, Acc_2};
walk_match_ast(Match_ast, Acc) ->
	{Match_ast, Acc}.

%% if a record is not already matched, generate a var name for it.
-spec new_record_var_name(ast_record()) -> atom().
new_record_var_name(Record) ->
	Record_name_1 = ast_record_name(Record),
	[First|Tail] = atom_to_list(Record_name_1),
	Record_name_2 = [string:to_upper(First)|Tail],
	list_to_atom(
		Record_name_2 ++ "_" ++ integer_to_list(random:uniform(100000))
	).

%%
walk_tuple_ast({tuple, Ln_num, Elements}, Acc) ->
	{tuple, Ln_num, walk_ast(Elements, [], Acc)}.

%%
walk_var_ast({var, _Ln_num, Var_name} = Var, Acc) when element(1,Var) == var, ?is_dict(Acc) ->
	case dict:find(Var_name, Acc) of
		{ok, {Record_name, Field_name, Record_var}} ->
			merl:quote(field_name_getter_function_name(Record_name, Field_name) ++ "(" ++ atom_to_list(Record_var) ++ ")");
		error ->
			Var
	end.

%% A list
walk_cons_ast({cons, Ln_num, Head, Tail}, Acc) ->
	{cons, Ln_num, walk_ast(Head, [], Acc), walk_ast(Tail, [], Acc)}.

%%
walk_case_ast({'case', Ln_num, Expression, Clauses}, Acc) ->
	{'case', Ln_num, walk_ast(Expression,[],Acc), Clauses}.

%% Insert the functions which we will compile, just before the end of the module.
insert_function_asts(Functions, [{eof,_}] = EOF) ->
	Functions ++ EOF;
insert_function_asts(Functions, [AST|Tail]) ->
	[AST|insert_function_asts(Functions,Tail)].


%%% ============================================================================
%%% Creating Records
%%% ============================================================================

%% Creating records with default values
%%     Records that set fields will then set individual values over the defaults
%% One function for creating the  latest version
%% One function for creating a specific version
build_new_record_fns(Record_name, [], Version, Fields) ->
	Record_tuple = list_to_tuple(
		[Record_name, Version] ++ [ast_record_field_value(F, undefined) || F <- Fields]),
	Function_erlang = lists:flatten(io_lib:format(
		"aversion_new_~p_latest() -> ~p.", [Record_name,Record_tuple]
	)),
	merl:quote(Function_erlang);
build_new_record_fns(Record_name, [Rec|Tail], _, Fields) ->
	Version = ast_record_attribute_version(Rec),
	%% first fields is always version
	[_|Rec_fields] = ast_record_attribute_fields(Rec),
	build_new_record_fns(Record_name, Tail, Version, Fields++Rec_fields).






