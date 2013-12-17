%%%-------------------------------------------------------------------
%%% File    : ovid_table_owner.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%%
%%% Created : 17 Dec 2013 by Thomas Lindgren <>
%%%-------------------------------------------------------------------

%% Own a collection of tables. This is a generic sort of service,
%% and you can start several of them.
%%
%% Spec of tables
%%   {Tab, Opts}
%%   {Tab, Loc, Opts}   Loc is the filename or directory of the table
%%
%% UNFINISHED
%% - dynamic add tables (and delete?)
%% - dynamic save/restore tables
%% - table handover by messaging ...
%% - dets tables

-module(gen_table_owner).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {tables=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%%
%% UNFINISHED
%% - always started as {local, Server}, are the other cases relevant?
%% - any startup options of interest here?

start_link(Server, TabSpecs) ->
    gen_server:start_link({local, Server}, ?MODULE, [TabSpecs], []).

new(Server, TabSpec) ->
    gen_server:call(Server, {create, TabSpec}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

%% Setup the tables, then loop for messages

init(TabSpecs) ->
    setup_tables(TabSpecs),
    {ok, #state{tables=TabSpecs}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%%
%% UNFINISHED
%% - dynamic setup of tables
%%   * if table exists, just return
%%   * if table does not exist, create it and add to tables

handle_call({create, TabSpec}, _From, State) ->
    case table_exists(TabSpec, State#state.tables) of
	true ->
	    {reply, ok, State};
	false ->
	    setup_table(TabSpec),
	    {reply, ok, 
	     State#state{
	       tables=State#state.tables++[TabSpec]}}
    end;
handle_call(_Request, _From, State) ->
    Reply = {error, unknown_message},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------

terminate(_Reason, State) ->
    save_tables(State#state.tables),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

table_exists({Tab, _Opts}, Tabs) ->
    case lists:keysearch(Tab, 1, Tabs) of
	{value, _} ->
	    true;
	_ ->
	    false
    end;
table_exists({Tab, _Loc, _Opts}, Tabs) ->
    case lists:keysearch(Tab, 1, Tabs) of
	{value, _} ->
	    true;
	_ ->
	    false
    end.

%%

setup_tables(Tabs) ->
    lists:foreach(fun setup_table/1, Tabs).

save_tables(Tabs) ->
    lists:foreach(fun save_table/1, Tabs).

%% {Tab, Opts} = just create the table
%% {Tab, Loc, Opts} = try to read table from Loc,
%%   otherwise create it
%%
%% - assumes no extra info needs to be saved regarding
%%   table

setup_table({Tab, Opts}) ->	      
    ets:new(Tab, Opts);
setup_table({Tab, Loc, Opts}) ->
    case try_restore_table(Tab, Loc) of
	true ->
	    ok;
	false ->
	    ets:new(Tab, Opts)
    end;
setup_table(OtherSpec) ->
    exit({unknown_table_spec, OtherSpec}).

save_table({Tab, Loc, Opts}) ->
    try_save_table(Tab, Loc);
save_table(OtherSpec) ->
    ok.

%% Try to locate the table at path Loc and open it. 
%% If the table opens, okay. If it's another table, die.
%% If the table does not exist, return false. (It will
%% be created instead.)
%%
%% UNFINISHED
%% - should we really exit? probably, but check

try_restore_table(Tab, Loc) ->
    case ets:file2tab(Loc) of
	{ok, Tab} ->
	    true;
	{ok, OtherTab} ->
	    exit({restore_error, {wanted, Tab},{got, OtherTab, Loc}});
	Err ->
	    NewLoc = filename:join([Loc, atom_to_list(Tab)++".ets"]),
	    case ets:file2tab(NewLoc) of
		{ok, Tab} ->
		    true;
		{ok, OtherTab} ->
		    exit({restore_error, {wanted, Tab},{got, OtherTab, NewLoc}});
		OtherErr ->
		    false
	    end
    end.

%% Try to save the table if possible. If table can't be saved, exit with
%% the error.
%%
%% (This can be the case if the directory does not exist for example.)
%%
%% UNFINISHED
%% - should we create the location (a la "mkdir -p") if nonexistent?
%% - should we really exit?

try_save_table(Tab, Loc) ->
    case ets:tab2file(Tab, filename:join([Loc, atom_to_list(Tab)++".ets"])) of
	ok ->
	    ok;
	Err ->
	    exit(Err)
    end.

	    



