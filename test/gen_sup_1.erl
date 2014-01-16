%%% File    : gen_sup_1.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created : 25 Dec 2013 by Thomas Lindgren <>

%% Set up all varieties of supervisors
%% Set up many cases of deep supervisor trees
%% check that supervisors exist
%% check that supervisors are restarted
%%  according to configs
%%  - shallow/single restart
%%  - deep restart

-module(gen_sup_1).
-include_lib("eunit/include/eunit.hrl").

