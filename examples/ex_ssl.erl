%%% File    : ex_ssl.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created : 20 Jan 2014 by Thomas Lindgren <>

%% Example module, reimplementing the supervisor
%% tree of the ssl app in a few ways.
%%
%% What we can see:
%% - set env keys easily
%%   * current looks a bit awkward
%%   * optional key {opt_key, foo} should
%%     disappear from list if undefined
%%   * special case: specify env list
%%     {key, foo}, {opt_key, bar}, {key, baz}
%%     => [{foo, foovalue}, {baz, undefined}]
%%     => [{foo, foovalue}, {bar, barvalue}, {baz, undefined}]
%%     (+ read from application env)
%% - use env keys for app/sup parameters
%%   * well-known names, always expand using env
%%     maxR, maxT, restart_strategy, ...
%% - simple_one_for_one needs extra TLC too

-module(ex_ssl).
-compile(export_all).

%% Simple integration: keep the sub-supervisors unchanged.

app_1() ->
    {ssl, ssl_sup()}.

%% Wrap the child specs in the env keys

ssl_env(Tree) ->
    {env,
     [{session_cb, application:get_env(ssl, session_cb)},
      {session_cb_init_args, application:get_env(ssl, session_cb_init_args)},
      {session_lifetime, application:get_env(ssl, session_lifetime)}],
     Tree}.

ssl_sup() ->
    {sup, ssl_sup, {one_for_all, 10, 3600}, 
     [session_and_cert_manager_child_spec(),
      connection_manager_child_spec()]}.

session_and_cert_manager_child_spec() ->
    %% NB: session_lifetime is optional, how to spec?
    Opts = [{session_cb, {key, session_cb}},
	    {session_cb_init_args, {key, session_cb_init_args}},
	    {session_lifetime, {key, session_lifetime}}],
    Name = ssl_manager,  
    StartFunc = {ssl_manager, start_link, [Opts]},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

connection_manager_child_spec() ->
    Name = ssl_connection,  
    StartFunc = {ssl_connection_sup, start_link, []},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [ssl_connection],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

%% The following is ultimately started by ssl_connection_sup

ssl_connection_sup_init(_O) ->
    RestartStrategy = simple_one_for_one,
    MaxR = 0,
    MaxT = 3600,
   
    Name = undefined, % As simple_one_for_one is used.
    StartFunc = {tls_connection, start_link, []},
    Restart = temporary, % E.g. should not be restarted
    Shutdown = 4000,
    Modules = [tls_connection],
    Type = worker,
    
    ChildSpec = {Name, StartFunc, Restart, Shutdown, Type, Modules},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Here we specify the following tree directly
%%
%%  APP:ssl -> ssl_sup -> ssl_manager:start_link()
%%                     -> ssl_connection_sup -> tls_connection:start_link()
%%
%% LIMITATIONS:
%% - does not pass the Opts to ssl_manager!
%% - the Shutdown of 4000 is not given, so not strictly equivalent
%%
%% UNFINISHED
%% - does not pass the Opts to ssl_manager!
%% - the restart strategy params MAY not be given correctly, check this
%% - expand and check

app_2() ->
    {ssl, ssl_sup_2()}.

ssl_sup_2() ->
    {sup, ssl_sup, {one_for_all, 10, 3600}, 
     [ssl_manager,  %% <-- UNFINISHED
      {sup, ssl_connection_sup,
       {simple_one_for_one, 0, 3600},
       [{undefined, {tls_connection, start_link, []}, 
	 temporary, 4000, worker, [tls_connection]}]}]}.


    
