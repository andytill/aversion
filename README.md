# Aversion

Aversion is a parse transform for handling multiple versions of an erlang record, while still allowing the record unpack syntax.

The same record is deifined multiple times, the version field must be the first field. Other field names may not be duplicated. Each record version includes all of the fields of the previous versions.

```erlang
%% the original definition for the project record, defined for an
%% old version of our application
-record(project, {
    version = 1,
    name :: binary()
}).

%% a new 'migration', project records with a value of 2 in the version will will have a
%% commits field, the tuple will be three elements.
-record(project, {
    version = 2,
    commits = 0 :: integer()
}).
```

A parse transform will use these record definitions to auto generate functions that can manipulate different versions of the records.

```erlang
print_name(#project{ name = Name }) ->
    io:format(Name).
```

The code above will be writable with the parse transform.

```erlang
print_name(Project) ->
    Name = project_name(Project),
    io:format(Name).

%% this example doesn't need to check the version, but it could do
%% version specific checks if we need to
project_name(P) when element(1, P) == project ->
    element(2, P).
```

It will get generated into this code, above.  Additionally the parse transform could generate utility functions.

+ upgrade/downgrade a record between versions.
+ check if a version is supported.

A migration might even modify record field values.

### Creating a record that has migrations

Code will be rewritten to create a tuple with the correct schema for the version specified in the version field. The code below could be written in the same module.

```erlang
%% version 1
{project, 1, <<"erlyberly">>} = #project{ version = 1, name = <<"erlyberly">> }.

%% version 2, this has the commits field at element 4 which is defaulted to 0
{project, 2, <<"erlyberly">>, 0} = #project{ version = 2, name = <<"erlyberly">> }.
```

This is useful because a node might be aware that the cluster only supports version 1 even if this node can support version 2.

### Scenarios

##### Record version is higher than what is known

If the fields that are accessed are in the record version that we know, then the just access those fields and leave the other fields alone.  This is the likely situation, since we won't be using fields we're not aware of.

If we wish to modify those fields, just set elements and keep the fields we are not aware of.  The version value is not changed.

##### Record version does not have a field in a newer version

If the field is being unpacked and the field has a default value, then return the default value. If it does not have a default value it must throw an exception. It is highly recommended to set a default value for every new field.

If the record does not have a field and we want to write the field version to it, then the record must be upgraded to the version that field was defined in.

##### The node must send records of a specific version because the cluster has mixed versions

Specifying the `version` field in a record will create a record of that version.

riak should know the version number that can be created using capabilities.

If a record needs to be downgraded in a way that results in data loss then the request that initiated the work must be rejected as not supported by the cluster.

### Worked Example

##### Currently in riak\_core, record `riak_vnode_req_vX`

`riak_core` defines two different versions of record for [fold requests](https://github.com/basho/riak_core/blob/develop/include/riak_core_vnode.hrl#L24).

```erlang
-record(riak_core_fold_req_v1, {
          foldfun :: fun(),
          acc0 :: term()}).
-record(riak_core_fold_req_v2, {
          foldfun :: fun(),
          acc0 :: term(),
          forwardable :: boolean(),
          opts = [] :: list()}).


-define(VNODE_REQ, #riak_vnode_req_v1).
-define(COVERAGE_REQ, #riak_coverage_req_v1).
-define(FOLD_REQ, #riak_core_fold_req_v2).
```

And an upgrade function in [riak_core_util.erl](https://github.com/basho/riak_core/blob/develop/src/riak_core_util.erl#L599).

```erlang
%% @doc Convert a #riak_core_fold_req_v? record to the cluster's maximum
%%      supported record version.

make_fold_req(#riak_core_fold_req_v1{foldfun=FoldFun, acc0=Acc0}) ->
    make_fold_req(FoldFun, Acc0, false, []);
make_fold_req(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0,
                       forwardable=Forwardable, opts=Opts}) ->
    make_fold_req(FoldFun, Acc0, Forwardable, Opts).

make_fold_req(FoldFun, Acc0) ->
    make_fold_req(FoldFun, Acc0, false, []).

make_fold_req(FoldFun, Acc0, Forwardable, Opts) ->
    make_fold_reqv(riak_core_capability:get({riak_core, fold_req_version}, v1),
                   FoldFun, Acc0, Forwardable, Opts).

%% @doc Force a #riak_core_fold_req_v? record to the latest version,
%%      regardless of cluster support

make_newest_fold_req(#riak_core_fold_req_v1{foldfun=FoldFun, acc0=Acc0}) ->
    make_fold_reqv(v2, FoldFun, Acc0, false, []);
make_newest_fold_req(?FOLD_REQ{} = F) ->
    F.

%% @private
make_fold_reqv(v1, FoldFun, Acc0, _Forwardable, _Opts)
  when is_function(FoldFun, 3) ->
    #riak_core_fold_req_v1{foldfun=FoldFun, acc0=Acc0};
make_fold_reqv(v2, FoldFun, Acc0, Forwardable, Opts)
  when is_function(FoldFun, 3)
       andalso (Forwardable == true orelse Forwardable == false)
       andalso is_list(Opts) ->
    ?FOLD_REQ{foldfun=FoldFun, acc0=Acc0,
              forwardable=Forwardable, opts=Opts}.
```

There is no downgrade function. This is not required like it is in Riak TS because the records are not persisted in the metadata like the DDL is. Capabilities are used to determine which version of the record should be created. Defaults are used to upgrade the record but are 

When the vnode receives a `v1` version of the fold record it upgrades. The upgrade happens at the edge of the system so that the record is always at the current 

```erlang
handle_command(#riak_core_fold_req_v1{} = ReqV1,
               Sender, State) ->
    %% Use make_fold_req() to upgrade to the most recent ?FOLD_REQ
    handle_command(riak_core_util:make_newest_fold_req(ReqV1), Sender, State);
handle_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0,
                         forwardable=_Forwardable, opts=Opts}, Sender, State) ->
    %% The riak_core layer takes care of forwarding/not forwarding, so
    %% we ignore forwardable here.
    %%
    %% The function in riak_core used for object folding expects the
    %% bucket and key pair to be passed as the first parameter, but in
    %% riak_kv the bucket and key have been separated. This function
    %% wrapper is to address this mismatch.
    FoldWrapper = fun(Bucket, Key, Value, Acc) ->
                          FoldFun({Bucket, Key}, Value, Acc)
                  end,
    do_fold(FoldWrapper, Acc0, Sender, Opts, State);
```

##### Using Aversion

The records need to be defined with the same name, with version fields. Macros for record names is unnecessary because the record name is always the same.

```erlang
-record(riak_core_fold_req, {
          version = 1,
          foldfun :: fun(),
          acc0 :: term()}).
-record(riak_core_fold_req, {
          version = 2,
          forwardable = false :: boolean(),
          opts = [] :: list()}).
```

The `forwardable` has been given the default that it would be created with, with in the upgrade function.

The vnode command handling will look like this.

```erlang
handle_command(#riak_core_fold_req{foldfun=FoldFun, acc0=Acc0,
                                   forwardable=_Forwardable, 
                                   opts=Opts}, Sender, State) ->
    %% The riak_core layer takes care of forwarding/not forwarding, so
    %% we ignore forwardable here.
    %%
    %% The function in riak_core used for object folding expects the
    %% bucket and key pair to be passed as the first parameter, but in
    %% riak_kv the bucket and key have been separated. This function
    %% wrapper is to address this mismatch.
    FoldWrapper = fun(Bucket, Key, Value, Acc) ->
                          FoldFun({Bucket, Key}, Value, Acc)
                  end,
    do_fold(FoldWrapper, Acc0, Sender, Opts, State);
```

If the `riak_core_fold_req` version fields has a value of 1 then forwardable and opts will be given the default values specified in the record. If it is version 2 then the the record values will be used.

Creating the record can be done by using the latest version, and have nodes that don't understand this version use the fields that their version allows. If no version is specified, the latest version is assumed.

Or, use capabilities to create the version that the cluster fully supports. This is similar to the `make_fold_req/4` function above.

```erlang
VersionNumber = riak_core_capability:get({riak_core, fold_req_version}, 1),
#riak_core_fold_req{ version=VersionNumber, foldfun=FoldFun, acc0=Acc0 }.
```

**NOTE:** how to set fields on a record version that doesn't support those fields?  The example in riak_core just drops them, need to think what is best.

### Utility Functions

##### `is_safely_downgradable/1`

```erlang
is_safely_downgradable(NewVersion, Record::tuple()) -> boolean().
```

If the fields in the record that were added are equal to the default values then the record is safe to downgrade because no data is lost.

##### `upgrade/2`

Add default values for fields that were added from the given record version to target version.

##### `downgrade/2`

Throws an error if the record is not safely downgradable.

##### `force_downgrade/2`

YOLO downgrade, chop the record fields that do not exist in the requested version.

### Rewrites

```erlang
-record(myrec, { a }).v
```

##### Versioned record in function head

```erlang
my_function(#myrec{ a = A }) ->
    io:format("~p",[A]).
```

```erlang
my_function(MyRec) when element(1,MyRec) == myrec ->
    A = get_myrec_a(MyRec),
    io:format("~p",[A]).
```

##### Versioned record field used in function guard

```erlang
my_function(#myrec{ a = A }) when is_atom(A) ->
    io:format("~p",[A]).
```

```erlang
my_function(MyRec) when element(1,MyRec) == myrec andalso
                        is_atom(element(3,MyRec)) ->
    A = get_myrec_a(MyRec),
    io:format("~p",[A]).
```

##### `is_record/2` in function guard

```erlang
my_function(MyRec) when is_record(MyRec, myrec) ->
    io:format("~p",[MyRec]).
```

```erlang
my_function(MyRec) when element(1,MyRec) == myrec ->
    io:format("~p",[MyRec]).
```

##### Versioned record in fun

##### Versioned record in case clause

##### Versioned record access record field

```erlang
MyRec#myrec.a.
```
##### Versioned record access record field position

```erlang
#myrec.a.
```





