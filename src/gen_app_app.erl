%%% File    : gen_app_app.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created :  3 Mar 2014 by Thomas Lindgren <>

%% Refactored the gen_app boilerplate module into here.
%% This is just so we can use gen_app:start/2.

-module(gen_app_app).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1, prep_stop/1, config_change/3]).

-define(info(Str, Xs), error_logger:info_msg(Str, Xs)).

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
%% DESIGN NOTE
%%  We currently require that 'start_phases' is undefined (if you need
%% start_phases, writing a conventional module makes sense). From the
%% 'application' module man page (R16 or earlier), this implies that
%% a failover will ALWAYS start with reason 'normal'. If you call
%% application:takeover/2, the start reason will be {takeover, PrevNode}.
%%
%% NOTES
%% - currently only handles the simple case returning {ok, PID}.
%%
%% UNFINISHED (extensions)
%% - pass optional functions to run in the normal/takeover/failover cases
%%   optional {takeover, MFA} and suchlike
%% - pass optional state function or state tuple
%% - clean up the StartArgs before passing them to gen_sup:start_link/1?
%%   * might want to generalize

start(normal, StartArgs) ->
    SupArgs = StartArgs,
    gen_sup:start_link(SupArgs);
start({takeover, PrevNode}, StartArgs) ->
    ?info("Takeover (from ~p): ~w\n", [PrevNode, StartArgs]),
    SupArgs = StartArgs,
    gen_sup:start_link(SupArgs);
start({failover, PrevNode}, StartArgs) ->
    ?info("FAILOVER (from ~p): ~w\n", [PrevNode, StartArgs]),
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

%%--------------------------------------------------------------------
%% config_change/3
%%
%% (We have no access to State, so we can't lookup a function to use.)

config_change(Changed, New, Removed) ->
    ok.
