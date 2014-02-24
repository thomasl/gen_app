%%% File    : gen_sup_bridge.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created : 24 Feb 2014 by Thomas Lindgren <>

%% Provides supervisor_bridge for gen_sup
%%
%% (See childspec {bridge, Name, MFArgs}.)

-module(gen_sup_bridge).
-behaviour(supervisor_bridge).

-export([init/1, terminate/2]).

%%

init([{M, F, Args}]) ->
    PID = proc_lib:spawn_link(erlang, apply, [M, F, Args]),
    {ok, PID, {bridge_pid, PID}}.

%%

terminate(shutdown, {bridge_pid, PID}) ->
    %% supervisor originated
    exit(PID, kill);
terminate(_Rsn, {bridge_pid, PID}) ->
    %% other
    exit(PID, kill).




