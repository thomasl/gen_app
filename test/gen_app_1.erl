%%% File    : gen_app_1.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created : 25 Dec 2013 by Thomas Lindgren <>

%% Tests for gen_app
%%
%% Note: we use only basic supervisors here, see gen_sup_1 for
%% detailed supervisor tests
%%
%% UNFINISHED
%% - eunit compatibility ...

-module(gen_app_1).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local applications
%%
%% - start app with simple supervisor tree
%% - check keys: description, vsn, version, modules, registered,
%%    applications, included_applications, env, maxT, maxP,
%%    supervisor, sup (required)
%%
%% UNFINISHED
%% - eunit compliance needed ...

appsup_test() ->
    ?debugFmt("appsup test", []),
    app_setup(testapp1, 
	      [

required_keys_test() ->
    %% foreach
    exit(nyi).

optional_keys_test() ->
    %% foreach
    exit(nyi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Distributed applications
%%
%% - none yet, distr gen_app not implemented

distr_test() ->
    ?debugFmt("Distributed gen_app: skipped.\n", []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Error cases
%%
%% - check that key mod detected and errors
%% - check that key start_phases detected and errors
%% - check that keys supervisor or sup missing detected and errors

forbidden_keys_test() ->
    %% foreach
    exit(nyi).

missing_required_test() ->
    %% foreach
    exit(nyi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

app_setup(App, StartArgs) ->
    application:unload(App),
    gen_app:app_sup(App, StartArgs).
