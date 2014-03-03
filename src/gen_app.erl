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
%%   {mod, {gen_app_app, SupArg}},   %% NOTE module name!
%%   ...]}
%%
%% gen_app_app implements the static application behaviour.
%% (We do this because we want to provide gen_app:start/2 for
%% dynamic application startup)
%%
%% Where SupArg is a supervisor tree, specified as in module gen_sup.
%%
%% Option 2: start the application dynamically
%%
%%   gen_app:start(AppName, Args)
%%   gen_app:app_sup(AppName, Args)  [deprecated]
%%
%% where Args is an application description (man application)
%% extended with keys 'distributed' to show how to start, and
%% 'supervisor' or 'sup' to specify the supervisor tree.
%%
%% DESIGN NOTE
%% - we use application:ensure_all_started/1 by default rather than
%%   application:start/2, for convenience.
%%   * note that we use 'temporary' as the restart type;
%%     is it worth generalizing this?
%% - distributed app takeover/failover/... not yet handled
%% - start_phases not yet handled
%%   * might be better to just write an application module
%%     in this case => WONTFIX
%% 
%% STATUS
%% - needs testing of refactoring and use of gen_app_app
%% - upgrade to handle more complex cases, data driven again
%%   * multinode, failover, takeover, ...?
%%   * do that in gen_app_app? and/or here

-module(gen_app).
-export(
   [start/2,
    start/3,
    stop/1
   ]).
%% Deprecated
-export(
   [app_sup/2]
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-define(dbg(Str, Xs), io:format(Str, Xs)).
%-define(dbg(Str,Xs), ok).

-define(info(Str, Xs), error_logger:info_msg(Str, Xs)).

%% The 'application' module to use

-define(GEN_APPL, gen_app_app).

-define(exit(Rsn), erlang:exit({?MODULE, ?LINE, Rsn})).

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
    io:format("~p:app_sup/2 is deprecated, use ~p:start/2 instead\n",
	      [?MODULE, ?MODULE]),
    start(App, Args).

%% Main entry point

start(App, Args) ->
    start(App, normal, Args).

%% Advanced entry point
%% - similar to application start/2 callback
%%
%% In essence, using Args we build a dynamic application specification
%% with the name App, load it and start it. The actual application
%% module is given as the 'mod' key below (we currently use gen_app_app).
%% 
%% UNFINISHED
%% - currently limited to 'normal' startup
%%   * specify takeover/failover/etc actions; how?
%% - LoadType and the 'distributed' key is a bit awkward,
%%   can we spec it better?
%% - testing of distributed load/start needed

start(App, normal, Args) ->
    AppSpec = app_spec(App, Args),
    LoadType = case val(distributed, Args, local) of
		   true ->
		       distributed;
		   false ->
		       local;
		   Nodes when is_list(Nodes) ->
		       {distributed, Nodes};
		   {Time, Nodes} when is_list(Nodes) ->
		       {distributed, Time, Nodes};
		   Other ->
		       Other
	       end,
    start_app_sup(App, AppSpec, LoadType);
start(App, StartType, Args) ->
    exit({start_type_not_yet_handled, StartType}).

start_app_sup(App, AppSpec, LoadType) ->
    case application_loaded(App) of
	true ->
	    {error, {application_already_loaded, App}};
	false ->
	    case load_app(App, AppSpec, LoadType) of
		ok ->
		    application:ensure_all_started(App);
		Err ->
		    Err
	    end
    end.

%% App = app name
%% AppSpec = application spec
%% LoadType = 
%%   local                         (only on this node)
%%   distributed                   (distributed, use defaults)
%%   {distributed, Nodes}          (distributed on nodes, Time=0)
%%   {distributed, Time, Nodes}    (distributed on nodes, wait Time ms and restart)
%%   default                       (use default from conf, see man page)
%%
%% See the application:load/2 man page for more info on what goes on and
%% what parameters are being passed. The above provides some wrappers
%% to simplify the specification.
%%
%% 'distributed' loads the application in distributed mode
%% as per the application:load/2 man page. Not passing any arguments
%% means we allow the app to restart on any available node, otherwise
%% transplant to docs.
%%
%% NOTES:
%% - not sure if 'distributed' w/o args is useful?
%% - nicer notation for Nodes spec? API is very bare bones

load_app(App, AppSpec, local) ->
    application:load(AppSpec);
load_app(App, AppSpec, LoadType0) ->
    LoadType =
	case LoadType0 of
	    default ->
		default;
	    distributed ->
		%% - app starts on current node
		%% - app can failover to any CURRENTLY connected node
		Nodes = 
		    case nodes() of
			[] ->
			    [node()];
			Ns -> 
			    [node()|list_to_tuple(Ns)]
		    end,
		{App, Nodes};
	    {distributed, Nodes} ->
		{App, Nodes};
	    {distributed, Time, Nodes} 
	      when is_integer(Time), Time > 0 ->
		{App, Time, Nodes};
	    LoadType0 ->
		exit({load_type_not_handled, LoadType0})
	
	end,
    application:load(AppSpec, LoadType).

%% app_spec/2 creates a suitable application specification from the
%% key-value inputs. 

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
			{mod, {?GEN_APPL, SupArgs}},
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stop/1 just uses gen_app_app.
%%
%% NOTE: interface may need some work.
%%
%% UNFINISHED
%% - testing

stop(State) ->
    gen_app_app:stop(State).

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

