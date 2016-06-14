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
    %% create a dict of the record names, tagged with `aversion_record` for
    %% namespacing, since this dict will also contain the variable names for fields
    Record_names = [{{aversion_record, R}, ok} || R <- dict:fetch_keys(Aversion_defs)],
	AST_out_1 = walk_ast(AST_in, [], dict:from_list(Record_names)),
    AST_out_2 = filter_aversion_records(Aversion_defs, AST_out_1),
    AST_out_3 = insert_record_getter_functions(Function_list, AST_out_2),
	%% debug
    %% io:format("~n== AST OUT ==~n~p~n== AST OUT ==~n~n", [AST_out_3]),
    AST_out_3.

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
build_record_unpack_field_functions(N, [Record|Tail], Function_list_1) ->
	Record_name = ast_record_attribute_name(Record),
	Fields = ast_record_attribute_fields(Record),
	Fn =
		fun(Field_x, Acc) ->
			Field_name = ast_record_field_name(Field_x),
			Fn_key = {Record_name, Field_name},
			case lists:keymember(Fn_key, 1, Function_list_1) of
				false ->
					Fn_erlang = build_unpacker_function(Record_name, N, Field_name),
					[{Fn_key, Fn_erlang} | Acc];
				true ->
					Acc
			end
		end,
	Function_list_2 = lists:foldl(Fn, [], Fields),
	build_record_unpack_field_functions(N+1, Tail, Function_list_1 ++ Function_list_2).

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
ast_var_name({var,_,Var_name}) ->
	Var_name.

%% Creates a var, which is a variable name in erlang AST
new_ast_var(Ln_num, Var_name) when is_integer(Ln_num), is_atom(Var_name) ->
	{var, Ln_num, Var_name}.


%%% ============================================================================
%%% AST iteration
%%% ============================================================================

% do_the_thing(#myrec{ field1 = F }) ->
% 	{hi, F}.

%% ORIGINAL AST...

% {function,19,do_the_thing,1,
%      [{clause,19,
%           [{record,19,myrec,
%                [{record_field,19,{atom,19,field1},{var,19,'F'}}]}],
%           [],
%           [{tuple,20,[{atom,20,hi},{var,20,'F'}]}]}]}

%% TO...

% {function,22,do_the_thing_rewrite,1,
%      [{clause,22,
%           [{var,22,'MyRec'}],
%           [[{call,22,{atom,22,element},[{integer,22,1},{var,22,'MyRec'}]}]],
%           [{tuple,23,
%                [{atom,23,hi},
%                 {call,23,
%                     {atom,23,element},
%                     [{integer,23,3},{var,23,'MyRec'}]}]}]}]}

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
			{AST_result, Acc_2} = walk_record_ast(Rec, Record_var_name, Acc_1);
		false ->
			AST_result = Rec,
			Acc_2 = Acc_1
	end ,
	walk_function_args_ast(Tail, [AST_result|AST_out], Acc_2);
walk_function_args_ast([Other|Tail], AST_out, Acc) ->
	walk_function_args_ast(Tail, [Other|AST_out], Acc).

%%
-spec walk_record_ast(ast(), atom(), term()) -> {ast(), term()}.
walk_record_ast(Rec, Record_var_name, Acc_1) when is_atom(Record_var_name) ->
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

%%
walk_match_ast({match, _, Left, Right}, Acc_1) when element(1,Left) == record, element(1,Right) == var ->
	Var_name = ast_var_name(Right),
	{_, Acc_2} = walk_record_ast(Left, Var_name, Acc_1),
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
insert_record_getter_functions(Functions_1, [{eof,_}] = EOF) ->
	Functions_2 = [merl:quote(F) || {_,F} <- Functions_1],
	Functions_2 ++ EOF;
insert_record_getter_functions(Functions, [AST|Tail]) ->
	[AST|insert_record_getter_functions(Functions,Tail)].
