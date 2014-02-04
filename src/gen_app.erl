%%%-------------------------------------------------------------------
%%% File    : gen_app.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%%
%%% Created : 15 Dec 2013 by Thomas Lindgren <>
%%%-------------------------------------------------------------------

%% Implement a generic/data driven application.  This uses our generic
%% supervisor by default.
%%
%% Option 1: You can use this in multiple .app files; just plug
%% in the appropriate supervisor spec in the .app file:
%%
%% {application, App, 
%%  [...,
%%   {mod, {?MODULE, SupArg}},
%%   ...]}
%%
%% Where SupArg is a supervisor tree, specified as in module gen_sup.
%%
%% Option 2: start the application dynamically
%%
%%   ?MODULE:app_sup(AppName, Args)
%%
%% where Args is an application description (man application)
%% extended with keys 'distributed' to show how to start, and
%% 'supervisor' or 'sup' to specify the supervisor tree.
%%
%% STATUS
%% - seems to work, esp. the app_sup/2 case
%% - upgrade to handle more complex cases, data driven
%%   * specify normal/takeover/failover funs
%%   * specify state tuple

-module(gen_app).
-behaviour(application).

-export([app_sup/2]).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-define(dbg(Str, Xs), io:format(Str, Xs)).
%-define(dbg(Str,Xs), ok).

-define(info(Str, Xs), error_logger:info_msg(Str, Xs)).

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% DYNAMICALLY start a fresh supervisor tree as an application. See
%% gen_sup for more info on supervisor tree specifications.
%%
%% Usage: ?MODULE:app_sup(my_app, [{sup, my_sup}])
%%  (simplest possible, creates supervisor my_sup)
%%
%% Note that Args here is an application description, extended
%% with keys for how to start ('distributed') and the supervisor
%% tree ('sup' or 'supervisor').

app_sup(App, Args) ->
    AppSpec = app_spec(App, Args),
    LoadType = val(distributed, Args, local),
    start_app_sup(App, AppSpec, LoadType).

start_app_sup(App, AppSpec, LoadType) ->
    case application_loaded(App) of
	true ->
	    {error, {application_already_loaded, App}};
	false ->
	    case LoadType of
		local ->
		    %% Start using load/1
		    case application:load(AppSpec) of
			ok ->
			    application:ensure_all_started(App);
			Err ->
			    Err
		    end;
		_ ->
		    %% Start using load/2
		    %%
		    %% - check: do we have to start on all nodes
		    %%   or is that implicit?
		    case application:load(AppSpec, LoadType) of
			ok ->
			    application:ensure_all_started(App);
			Err ->
			    Err
		    end
	    end
    end.

app_spec(App, KV_args) ->
    case none_val([start_phases,mod], KV_args) of
	true ->
	    SupArgs = val([supervisor, sup], KV_args),
	    case SupArgs of
		undefined ->
		    exit({no_supervisor_specified, KV_args});
		_ ->
		    {application, App,
		     optional(
		       [kval(description, KV_args, fmt("App/sup ~p", [App])),
			kval([vsn, version], KV_args, "0.0"),
			{mod, {?MODULE, SupArgs}},
			kval(modules, KV_args),
			kval(registered, KV_args),
			kval(applications, KV_args),
			kval(included_applications, KV_args),
			kval(env, KV_args),
			kval(maxT, KV_args),
			kval(maxP, KV_args)])}
	    end;
	false ->
	    case is_defined(start_phases, KV_args) of
		{true, Val} ->
		    exit({application_key_not_supported, {start_phases, Val}});
		false ->
		    case is_defined(mod, KV_args) of
			{true, Val} ->
			    exit({application_key_not_supported, {mod, Val}});
			false ->
			    %% Hmm, not sure; one of the keys was not
			    %% present, yet we could finf all of them
			    exit({internal_inconsistency, KV_args})
		    end
	    end
    end.

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
%%  Type = normal | {takeover, Node} | {failover, Node}
%%
%% NOTES
%% - currently only handles the simple case returning {ok, PID}.
%%
%% UNFINISHED (extensions)
%% - pass optional functions to run in the normal/takeover/failover cases
%% - pass optional state function or state tuple
%% - clean up the StartArgs before passing them to gen_sup:start_link/1

start(normal, StartArgs) ->
    SupArgs = StartArgs,
    gen_sup:start_link(SupArgs);
start({takeover, Node}, StartArgs) ->
    ?info("Takeover(from ~p): ~w\n", [Node, StartArgs]),
    SupArgs = StartArgs,
    gen_sup:start_link(SupArgs);
start({failover, Node}, StartArgs) ->
    ?info("FAILOVER(from ~p): ~w\n", [Node, StartArgs]),
    SupArgs = StartArgs,
    gen_sup:start_link(SupArgs);
start(Other, StartArgs) ->
    exit({unknown_startup_type, Other}).

%%--------------------------------------------------------------------
%% start_phase/3 is called when the application is
%% started in phases, which is specified in the .app file.
%%
%% NOTE:
%% - the use case currently seems complex enough that writing a
%%   custom application module is okay, so we skip this.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
%% - if a {stop, MF} is passed, run it on the State. The function
%%   return value is ignored, like stop/1 itself.

stop(State) ->
    case catch lists:keysearch(stop, 1, State) of
	{value, {_, {M, F}}} ->
	    (catch M:F(State));
	_ ->
	    ok
    end.

%%--------------------------------------------------------------------
%%  State is passed when {ok, Pid, State} is returned.
%%
%% We do this: pass a {prep_stop, MF} as part of state. If this
%% is present, we apply the MF to the state. Otherwise, pass the
%% state unchanged. 

prep_stop(State) ->
    case catch lists:keysearch(prep_stop, 1, State) of
	{value, {_, {M, F}}} ->
	    case catch M:F(State) of
		{'EXIT', Rsn} ->
		    State;
		NewState ->
		    NewState
	    end;
	_ ->
	    State
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% (Or should this just be keysearch/3 ...?)

is_defined(Key, List) ->
    case proplists:get_value(Key, List, undefined) of
	undefined ->
	    false;
	Val ->
	    {true, Val}
    end.

%% remove all optional keys (with value undefined).

optional(Xs) ->
    [ {Key, Val} 
      || {Key, Val} <- Xs,
	 Val =/= undefined ].

%% Wrap a key lookup in a {Key, Val} tuple

kval(Key, KVs) ->
    {key_of(Key), val(Key, KVs)}.

kval(Key, KVs, Dflt) ->
    {key_of(Key), val(Key, KVs, Dflt)}.

%% If key is a list of keys, we choose the
%% first one. Otherwise just the key.

key_of([K|_]) ->
    K;
key_of(K) ->
    K.

%% Lookup key in list of key-value args
%% - key must be constant or list
%% - if arg1 is a list, try each of args until
%%   one found; it's an "or-else"

val(K, KVs) ->
    val(K, KVs, undefined).

val([], KV_args, Default) ->
    Default;
val([Key|Keys], KV_args, Default) ->
    case val(Key, KV_args) of
	undefined ->
	    val(Keys, KV_args, Default);
	Val ->
	    Val
    end;
val(Key, KV_args, Default) ->
    proplists:get_value(Key, KV_args, Default).

%% None of the keys is defined in proplist KVs
%% - could probably be written more elegantly

none_val([K|Ks], KVs) ->
    case val(K, KVs) of
	undefined ->
	    none_val(Ks, KVs);
	_ ->
	    false
    end;
none_val([], _KVs) ->
    true.

%% Check whether App is loaded, returns bool()

application_loaded(App) ->
    case lists:keysearch(App, 1, application:loaded_applications()) of
	{value, _} ->
	    true;
	_ ->
	    false
    end.

%%

fmt(Str, Xs) ->
    lists:flatten(io_lib:format(Str, Xs)).

%%====================================================================
%% TESTS
%%====================================================================

test1(App, SupSpec) ->
    test1_setup(App, SupSpec).

%% (this is really a pattern for how to use this module ...)
%% - pass an AppSpec instead? with extra arg for starting supervisor
%% - should invoke load/2 to be general (get this from StartArgs?)

test1_setup(App, StartArgs) ->
    application:unload(App),
    app_sup(App, StartArgs).

