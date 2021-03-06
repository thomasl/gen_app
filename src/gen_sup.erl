%%%-------------------------------------------------------------------
%%% File    : gen_sup.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%%
%%% Created : 12 Dec 2013 by Thomas Lindgren <>
%%%-------------------------------------------------------------------
%%
%% Recursive supervisor starter. You specify all you want
%% started as a tree, and this guy sets it up. It works
%% by invoking itself to start supervisors, and starting
%% other children "normally".
%%
%% The supervisor and child specifications are expanded
%% by each supervisor as they start (lazy expansion) rather
%% than expanding them before starting any supervisor
%% (eager expansion).
%%
%% NB: failure inside the tree will be handled according
%% to enclosing supervisor strategy. 
%%
%% What's nice:
%% - you can hand an entire tree of things to this
%%   one and it all gets done
%% - the tree is data, rather than starting things
%%   inside code. So it can be manipulated.
%% - various defaults available
%%   and settable
%%
%% See test/1 below for some example supervisor strategies.
%% As an example, run
%%
%%  ?MODULE:start_link(?MODULE:test(1))
%%
%% (Note that the supervisors are not killed after the tests, so there
%%  may be some awkwardness in running several tests.)
%%
%% NOTE: test supervisor strategy 'simple_one_for_one',
%%  child spec + use start_child(SupRef, ExtraArgsList)
%%  - undecided whether this is a top use case? seems
%%    like a very hacky solution (better found in
%%    a library somewhere, right?)
%%
%% UNFINISHED
%% - verify (and error check) entire tree
%%   * reused names, repeated starts?
%% - recursive expand of tree directly for debug
%% - expand_term is used ad hoc, can we
%%   do it once and for all instead?
%%   * but avoid doing it again and again for subterms

-module(gen_sup).
-behaviour(supervisor).

%% API
-export(
   [
    start_link/1
   ]).

%% Supervisor callbacks
-export([init/1]).

%% Internal export
-export(
   [
    spawn_link_bridge/1,
    spawn_link_bridge/2
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Standard config keys
%%  full_supervisor_strategy: {restart, maxr, maxt}
%%  restart_strategy: one_for_one, one_for_all, etc
%%  max_restarts_in_period: "MaxR"
%%  restart_period: "MaxT" (in seconds)
%%  child_modules: list of modules or dynamic
%%  child_restart: permanent/temporary/...
%%  child_shutdown: brutal_kill/timeout() (in milliseconds)

-define(full_supervisor_strategy, supervisor_strategy).
-define(restart_strategy, restart_strategy).
-define(max_restarts_in_period, max_restarts_in_period).
-define(restart_period, restart_period).
-define(child_modules, child_modules).
-define(child_restart, child_restart).
-define(child_shutdown, child_shutdown).

%% Fallback default values

-define(fallback_strategy, one_for_one).  %% supervisor strategy
-define(fallback_max_r, 0).  %% max restarts before failing
-define(fallback_max_t, 1).  %% restart period in seconds

-define(fallback_restart, permanent).  
-define(fallback_shutdown, 2000).  %% Timeout in ms

%-define(dbg(Str, Xs), io:format(Str, Xs)).
-define(dbg(Str, Xs), ok).

-define(info(Str, Xs), error_logger:info_msg(Str, Xs)).

-define(table_owner, gen_table_owner).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------

%% This one links to the invoking process
%% - useful in a static hierarchy, e.g., call this in
%%   the enclosing application

start_link(SupSpec0) ->
    ?dbg("Expanding ~p\n", [SupSpec0]),
    {ServerSpec, SupArgs} = convert_server_spec(SupSpec0),
    ?info("Starting supervisor:start_link(~w, ~w,(~w)\n", [ServerSpec, ?MODULE, SupArgs]),
    supervisor:start_link(ServerSpec, ?MODULE, SupArgs).

%% This one starts as a child of Owner (which must be a supervisor)
%% - useful when you want to dynamically hook your tree
%%   into an existing supervision hierarchy

start_child(Owner, SupSpec) ->
    ?dbg("Expanding ~p\n", [SupSpec]),
    ChildSpec = convert_and_check_child_spec(SupSpec),
    ?info("Starting child (owner ~w) ~w\n", [Owner, ChildSpec]),
    supervisor:start_child(Owner, ChildSpec).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
%%
%% Note: the input here is generated by convert_server_spec/1
%% from the incoming args.

init({SupStrategy, ChildSpecs}) ->
    {ok, {SupStrategy, ChildSpecs}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The incoming SupSpec is our own simplified format.
%%    {supervisor, Name, SupStrategy?, [Child]?}
%%  | {sup, Name, SupStrategy?, [Child]?}
%%  | {env, PropList, Child}
%%  | {envlist, EnvKeyList, Child}
%%
%% where
%%  SupStrategy = Strategy | {Strategy, MaxR, MaxT}
%%  Child = Mod | {Name, Mod} 
%%    | SupSpec
%%    | {Id, StartMFArgs, Restart, Shutdown, Type?, Mods?}
%%  EnvKeyList = list of {key, Key} | {opt_key, Key} | atom()
%%    (see envlist/2 for how they are expanded)
%%
%% Note: if one of SupStrategy or [Child] is omitted, the
%%  remaining argument is assumed to be [Child].
%%  Thus, {sup, foo, [{sup, bar},{sup,baz}]}
%%   will spawn three supervisors foo, bar, baz.
%%
%% Note: occurrences of {key, Key, Default?} inside Mod, Name,
%%  Id, StartMFArgs, Restart, Shutdown, Type, Mods,
%%  Strategy, MaxR, MaxT are
%%  replaced by the value of Key in the current env.
%%   (or 'undefined' if none is found).
%% Note: occurrences of {keylist, Keys} replaced by list of
%%  key-value pairs; the Keys are same type as EnvKeyList.
%%
%% Default values:
%%   Default = undefined
%%   [Child]? => []
%%   SupStrategy? => {key, supervisor_strategy, SeeBelow}
%%   Restart = {key, child_restart, permanent}
%%   Shutdown = {key, child_shutdown, 1}
%%   Type = worker
%%   Mods = dynamic
%%
%% The output is:
%%  {ServerSpec, {SupStrategy, ChildSpecs}, Env}
%% where
%%  ServerSpec = 
%%     {local, Name} 
%%   | {global, Name} 
%%   | {via, Mod, Name}
%%  SupStrategy = {Strategy, MaxRestarts, MaxTimePeriod}
%%  ChildSpecs = <toplevel converted specs>
%%    {Id, StartMFArgs, Restart, Shutdown, Type, Modules}
%%   with these values as per the supervisor man page.
%%
%% The Env value is a proplist of config parameters
%% used to fill in defaults and explicit key reads.
%% If the key is not found in the env, we look for the key in 
%% the application and system config. If still not found,
%% return the default value.
%%
%% NB: For supervisor children, we propagate the env and
%%  rewrite into child specs that invoke this module
%%  as appropriate.
%%
%% Rewriting is done one level at a time, when the
%% supervisor starts. (Recursive rewriting would
%% process the tree many times. Though this might
%% not be an issue.)

convert_server_spec(Spec) ->
    convert_server_spec(Spec, empty_environment()).

%% First, we have a number of shorthand forms that
%% are converted into longhand spec. Then the longhand
%% is converted into an output spec.

convert_server_spec({sup, Name}, Env) ->
    convert_server_spec({supervisor, Name}, Env);
convert_server_spec({sup, Name, Childs}, Env) ->
    Strat = default_sup_strategy(Env),
    convert_server_spec({supervisor, Name, Strat, Childs}, Env);
convert_server_spec({sup, Name, Strat, Childs}, Env) ->
    convert_server_spec({supervisor, Name, Strat, Childs}, Env);
convert_server_spec({supervisor, Name}, Env) ->
    Strat = default_sup_strategy(Env),
    Childs = [],
    convert_server_spec({supervisor, Name, Strat, Childs}, Env);
convert_server_spec({supervisor, Name, Childs}, Env) ->
    Strat = default_sup_strategy(Env),
    convert_server_spec({supervisor, Name, Strat, Childs}, Env);
convert_server_spec({env, KVs, SupChild}, Env) ->
    NewEnv = extend_environment(KVs, Env),
    convert_server_spec(SupChild, NewEnv);
convert_server_spec({envlist, Keys, SupChild}, Env) ->
    NewEnv = extend_environment_list(Keys, Env),
    convert_server_spec(SupChild, NewEnv);
convert_server_spec({supervisor, Name, Strat, Childs0}, Env) ->
    %% OK, now we've gotten rid of all the shorthand,
    %% convert into proper output spec
    ServerSpec = sup_name_to_server(Name, Env),
    SupStrategy = convert_sup_strategy(Strat, Env),
    Childs = convert_childspecs(Childs0, Env),
    {ServerSpec, {SupStrategy, Childs}};
convert_server_spec(OtherSpec, Env) ->
    exit({bad_supervisor_spec, OtherSpec}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sup_name_to_server({global, Name}, Env) ->
    NewName = expand_term(Name, Env),
    {global, NewName};
sup_name_to_server({local, Name}, Env) ->
    NewName = expand_term(Name, Env),
    {local, NewName};
sup_name_to_server({via, Mod, Name}, Env) ->
    NewMod = expand_term(Mod, Env),
    NewName = expand_term(Name, Env),
    {via, NewMod, NewName};
sup_name_to_server(Name, Env) ->
    NewName = expand_term(Name, Env),
    {local, NewName}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_sup_strategy({_Strat, _MaxR, _MaxT}=SupStrat, Env) ->
    expand_term(SupStrat, Env);
convert_sup_strategy(Strat, Env) ->
    Strat = expand_term(Strat, Env),
    MaxR = read_config(max_restarts_in_period, Env, 0),
    MaxT = read_config(restart_period, Env, 1),
    {Strat, MaxR, MaxT}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_childspecs(Childs, Env) ->
    [ convert_and_check_child_spec(Child, Env)
      || Child <- Childs ].

%% Childspecs:
%% - supervisor specs
%% - {Name, Mod}
%% - Mod
%% - {Id, StartMFArgs, Restart, Shutdown, Type?, Mods?}
%% - {env, KVs, ChildSpec}
%%
%% UNFINISHED
%% - note the recursive expansion of {key,...}
%%   do we need to keep track of the keys we have
%%   seen to avoid trouble?
%%   (expansion of a term already does this, so that
%%   part is safe)
%%
%% MIGHT BE NICE
%% - allow full recursive expansion of a tree to see
%%   that everything is okay

convert_and_check_child_spec(Chspec) ->
    Env = [],
    convert_and_check_child_spec(Chspec, Env).

convert_and_check_child_spec(Chspec, Env) ->
    NewSpec = convert_child_spec(Chspec, Env),
    case supervisor:check_childspecs([NewSpec]) of
	ok ->
	    NewSpec;
	{error, Err} ->
	    exit({invalid_child_spec, Err, 
		  {original, Chspec}, 
		  {expands_to, NewSpec}, 
		  {env, Env}})
    end.

%% This is the main loop for converting child specs

convert_child_spec({sup, Name}, Env) ->
    convert_child_spec({supervisor, Name}, Env);
convert_child_spec({sup, Name, Strat}, Env) ->
    convert_child_spec({supervisor, Name, Strat}, Env);
convert_child_spec({sup, Name, Strat, Childs}, Env) ->
    convert_child_spec({supervisor, Name, Strat, Childs}, Env);
convert_child_spec({supervisor, Name}, Env) ->
    Strat = default_sup_strategy(Env),
    Childs = [],
    convert_child_spec({supervisor, Name, Strat, Childs}, Env);
convert_child_spec({supervisor, Name, Strat0}, Env) ->
    Strat = expand_term(Strat0, Env),
    Childs = [],
    convert_child_spec({supervisor, Name, Strat, Childs}, Env);
convert_child_spec({env, KVs, SupChild}, Env) ->
    NewEnv = extend_environment(KVs, Env),
    convert_child_spec(SupChild, NewEnv);
convert_child_spec({envlist, Keys, SupChild}, Env) ->
    NewEnv = extend_environment_list(Keys, Env),
    convert_child_spec(SupChild, NewEnv);
convert_child_spec({key, _Key}=Term, Env) ->
    convert_child_spec(expand_term(Term, Env), Env);
convert_child_spec({key, _Key, _Dflt}=Term, Env) ->
    convert_child_spec(expand_term(Term, Env), Env);
convert_child_spec({keylist, Keys}=Term, Env) ->
    convert_child_spec(expand_term(Term, Env), Env);
convert_child_spec({supervisor, Name, Strat, Childs}, Env) ->
    Mods = read_config(child_modules, Env, dynamic),
    convert_child_spec({supervisor, Name, Strat, Childs, Mods}, Env);
convert_child_spec({supervisor, Name, Strat, Childs, Mods0}, Env) ->
    %% OK, now we've gotten rid of all the shorthand,
    %% convert into proper output supervisor child spec
    NewName = expand_term(Name, Env),
    Id = NewName,
    SupStrat = Strat,  %% ??? probably ok?? or expand now?
    ExpChilds = maybe_convert_childspecs(Childs, Env),
    SupSpec = enclose_env(Env, {supervisor, NewName, SupStrat, ExpChilds}),
    StartMFArgs = {?MODULE, start_link, [SupSpec]},
    Restart = read_config(child_restart, Env, ?fallback_restart),
    Shutdown = read_config(child_shutdown, Env, ?fallback_shutdown),
    Type = supervisor,
    Mods = expand_term(Mods0, Env),
    {Id, StartMFArgs, Restart, Shutdown, Type, Mods};
convert_child_spec({tables, Server, TabSpecs}, Env) ->
    %% special form of child which starts and owns ets tables
    %% - each ets table looks like
    %%    {Tab, Opts}
    %%   or
    %%    {Tab, Loc, Opts}
    %% - Loc means table is restored/saved to Loc if exists
    NewTabSpecs = expand_term(TabSpecs, Env),
    NewServer = expand_term(Server, Env),
    Id = NewServer,
    StartMFArgs = {?table_owner, start_link, [Server, NewTabSpecs]},
    Restart = read_config(child_restart, Env, ?fallback_restart),
    Shutdown = read_config(child_shutdown, Env, ?fallback_shutdown),
    Type = worker,
    Modules = read_config(child_modules, Env, dynamic),  %% ???
    {Id, StartMFArgs, Restart, Shutdown, Type, Modules};
convert_child_spec({Name, Mod}, Env) ->
    %% convert into proper output child spec
    NewName = expand_term(Name, Env),
    NewMod = expand_term(Mod, Env),
    Id = NewName,
    StartMFArgs = {NewMod, start_link, []},
    Restart = read_config(child_restart, Env, ?fallback_restart),
    Shutdown = read_config(child_shutdown, Env, ?fallback_shutdown),
    Type = worker,
    Modules = read_config(child_modules, Env, dynamic),  %% ???
    {Id, StartMFArgs, Restart, Shutdown, Type, Modules};
convert_child_spec({bridge, Name, MFArgs}, Env) ->
    %% start a general process MFArgs, obey supervisor
    %% protocol
    %% - named process case
    NewMFArgs = expand_term(MFArgs, Env),
    NewName = expand_term(Name, Env),
    Id = NewName,
    StartMFArgs = {?MODULE, spawn_link_bridge, [NewName, NewMFArgs]},
    Restart = read_config(child_restart, Env, ?fallback_restart),
    Shutdown = read_config(child_shutdown, Env, ?fallback_shutdown),
    Type = worker,
    Modules = read_config(child_modules, Env, dynamic),  %% ???
    {Id, StartMFArgs, Restart, Shutdown, Type, Modules};
convert_child_spec({bridge, MFArgs}, Env) ->
    %% start a general process MFArgs, obey supervisor
    %% protocol
    %% - anonymous process case
    NewMFArgs = expand_term(MFArgs, Env),
    Id = undefined,
    StartMFArgs = {?MODULE, spawn_link_bridge, [NewMFArgs]},
    Restart = read_config(child_restart, Env, ?fallback_restart),
    Shutdown = read_config(child_shutdown, Env, ?fallback_shutdown),
    Type = worker,
    Modules = read_config(child_modules, Env, dynamic),  %% ???
    {Id, StartMFArgs, Restart, Shutdown, Type, Modules};
convert_child_spec({Id, StartMFArgs, Restart, Shutdown, Type, Modules}=Chspec, Env) ->
    %% here we have a regular child spec, no more recursion,
    %% just expand away the keys.
    expand_term(Chspec, Env);
convert_child_spec(Mod, Env) ->
    %% Mod should be an atom (but could be a key)
    Name = expand_term(Mod, Env),
    if
	is_atom(Name) ->
	    convert_child_spec({Name, Mod}, Env);
	true ->
	    exit({bad_child_name, Mod, {expands_to, Name}})
    end.

%% Start a generic process wrapped in supervisor_bridge
%%
%% (See gen_sup_bridge for details, basically apply the MFArgs.)

spawn_link_bridge(Name, MFArgs) ->
    ?dbg("Starting bridge ~p ~p\n", [Name, MFArgs]),
    supervisor_bridge:start_link(Name, gen_sup_bridge, [MFArgs]).

spawn_link_bridge(MFArgs) ->
    ?dbg("Starting anon bridge ~p ~p\n", [Name, MFArgs]),
    supervisor_bridge:start_link(gen_sup_bridge, [MFArgs]).

%% enclose_env/2: pass the environment along,
%% unless it's empty

enclose_env([], Subspec) ->
    Subspec;
enclose_env(Env, Subspec) ->
    {env, Env, Subspec}.

%% UNFINISHED
%% - how do we set ExpandMe? put/get ...? ugh
%%   * would prefer not to pass around a value

maybe_convert_childspecs(Childs, Env) ->
    case expand_mode() of
	eager ->
	    convert_childspecs(Childs, Env);
	_ ->
	    Childs
    end.

set_expand_mode(lazy) ->    
    put(expand_mode, lazy);
set_expand_mode(eager) ->
    put(expand_mode, lazy).

expand_mode() ->
    get(expand_mode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Expand all occurrences of 
%%   {key, Key, Dflt?}
%% using Env, inside the Term. Can also
%% use {envlist, Keys}
%%   which expands a list of
%%    {key, K} | {opt_key, K} into
%%   key-value pairs {K,V} (opt_key that
%%   is undefined is dropped)
%%
%% UNFINISHED
%% - handling of maps when they arrive (R17)
%% - special expansion of predefined atoms
%%   like maxR, maxT and others? (= always expand,
%%   even if outside key-wrapper) ugh, unclear,
%%   that means we can't use them without some
%%   escape mechanism

expand_term(Term, Env) ->
    Keys = [],
    expand_term(Term, Env, Keys).

expand_term({envlist, Keys}, Env, SeenKeys) ->
    envlist(Keys, Env, SeenKeys);
expand_term({key, Key}, Env, Keys) ->
    Dflt = undefined,
    read_config(Key, Env, Dflt, Keys);
expand_term({key, Key, Dflt}, Env, Keys) ->
    read_config(Key, Env, Dflt, Keys);
expand_term({keylist, Keys}, Env, SeenKeys) ->
    keylist(Keys, Env, SeenKeys);
expand_term(Term, Env, Keys) ->
    if
	is_tuple(Term) ->
	    %% this also catches records
	    list_to_tuple(
	      [ expand_term(T, Env, Keys) || T <- tuple_to_list(Term) ]
	     );
	Term == [] ->
	    [];
	is_list(Term) ->
	    [expand_term(hd(Term), Env, Keys)
	     |expand_term(tl(Term), Env, Keys)];
	true ->
	    Term
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% UNFINISHED
%% - do we need to expand twice or is that handled
%%   by expand_term now?

default_sup_strategy(Env) ->
    Dflt = {{key, ?restart_strategy, ?fallback_strategy},
	    {key, ?max_restarts_in_period, ?fallback_max_r},
	    {key, ?restart_period, ?fallback_max_t}},
    Term0 = expand_term(
	      {key, ?full_supervisor_strategy, Dflt},
	      Env),
    Term1 = expand_term(Term0, Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% UNFINISHED
%% - use maps when available
%% - may be worth simplifying env
%%   to avoid passing around masses of
%%   redundant key-value pairs

empty_environment() ->
    [].

extend_environment(KVs, Env) ->    
    KVs ++ Env.

%% expand the keys into key-value pairs
%% and add them to the environment

extend_environment_list(Keys, Env) ->
    envlist(Keys, Env) ++ Env.

%% read_config/3 expands the key recursively in
%% the environment. Includes expansion of keys
%% inside the term.
%%
%% Detects loops of definitions and dies.

read_config(Key, Env) ->
    read_config(Key, Env, undefined).

read_config(Key, Env, Dflt) ->
    read_config(Key, Env, Dflt, []).
    
read_config(Key, Env, Dflt, Keys) ->
    case lists:member(Key, Keys) of
	true ->
	    exit({looping_definition, Key, Keys});
	false ->
	    case read_conf(Key, Env, Dflt) of
		{key, NewKey} ->
		    read_config(NewKey, Env, Dflt, [Key|Keys]);
		Other ->
		    expand_term(Other, Env, Keys)
	    end
    end.

read_conf(Key, Env, Dflt) ->
    case proplists:get_value(Key, Env, undefined) of
	undefined ->
	    case application:get_key(Key) of
		undefined ->
		    case application:get_env(Key) of
			undefined ->
			    Dflt;
			{ok, Val} ->
			    Val
		    end;
		{ok, Val} ->
		    Val
	    end;
	Val ->
	    Val
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% envlist/2 is used to easily read values from config and extend the
%% environment.  See ex_ssl for usage; this kind of case appears in
%% legacy code conversion (for example).
%%
%% Here are the expansions in the envlist:
%%  {key, K} => {K, value(K)}
%%  {opt_key, K} => {K, value(K)} if defined, else remove from list
%%  K => {K, value(K)}  when K is an atom, it is assumed to be {key, K}.
%%
%% Thus, {opt_key, K} will only extend the environment if K has a defined value,
%% while {key, K} will always extend it, with 'undefined' if no value is
%% found.
%%
%% Net result: In the ex_ssl case, we can write a very compact spec:
%%  {envlist,
%%   [session_cb, session_cb_init_args, session_lifetime], Item}
%% instead of the long-form:
%%  {env, [{session_cb, {key, session_cb}},...], Item}
%% or the even longer version, similar to the original code:
%%  {env, [{session_cb, application:get_env(ssl, session_cb)},...], Item}
%%
%% We have a second use case: when we just want to produce a list of key-values.
%% This is written as a {keylist, Keys}, and expands to the corresponding list
%% (rather than extending the env). keylist uses the same code as envlist.

keylist(Keys, Env) ->
    envlist(Keys, Env).

keylist(Keys, Env, SeenKeys) ->
    envlist(Keys, Env, SeenKeys).

envlist(Keys, Env) ->
    envlist(Keys, Env, []).

envlist([{key, K}|Xs], Env, Keys) ->
    [{K, read_config(K, Env, undefined, Keys)}|envlist(Xs, Env, Keys)];
envlist([{opt_key, K}|Xs], Env, Keys) ->
    case read_config(K, Env, undefined, Keys) of
	undefined ->
	    envlist(Xs, Env, Keys);
	Val ->
	    [{K, Val}|envlist(Xs, Env, Keys)]
    end;
envlist([K|Xs], Env, Keys) when is_atom(K) ->
    %% shorthand for {key, K}
    [{K, read_config(K, Env, undefined, Keys)}|envlist(Xs, Env, Keys)];
envlist([K|_], _Env, _Keys) ->
    exit({invalid_envlist_key, K});
envlist([], Env, Keys) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

%% usage: ?MODULE:start_link(?MODULE:test(K))

test(1) ->
    {sup, foo};
test(2) ->
    {sup, foo, [{sup, bar},{sup, baz}]};
test(3) ->
    {sup, ovid_sup,
     [om_ets_owner]}.

