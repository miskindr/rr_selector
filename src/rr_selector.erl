%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2023, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a round robin selector. Given a set of valid Erlang types,
%%%  this selector distributes work in a  
%%% <a href="https://www.techtarget.com/whatis/definition/round-robin">
%%% round-robin</a> fashion.
%%%
%%% @end
%%
-module(rr_selctor).
-behaviour(gen_server).

%% API
-export([start/1,start/3,stop/0,next/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% Parameters: Identifiers - a list of any valid erlang terms. Often,
%% these are PIDs or other identifiers.
%%
%% Value: a 2-tuple consisting of _ok_ followed by the process ID of 
%% the spawned rr_selector gen_server.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(list()) -> {ok, pid()} | ignore | {error, term()}.
start(Identifiers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Identifiers], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%%
%% Parameters: Registration_type - local or global atoms.
%%             Name - the name under which this rr_selector is registered
%%             Identifiers - a list of any valid erlang terms. Often,
%% these are PIDs or other identifiers.
%%
%% Value: a 2-tuple consisting of _ok_ followed by the process ID of 
%% the spawned rr_selector gen_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Identifiers) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Identifiers, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the next identifier to be used from a rr_selector spawned
%% using the module name as the registered name for the rr_selector 
%% gen_server.
%%
%%
%% Parameters: None.
%%
%% Value: a valid Erlang type.
%%
%% @end
%%--------------------------------------------------------------------

-spec next() -> term().
next()-> gen_server:call(?MODULE, next_id).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the next identifier to be used from a rr_selector spawned
%% using a specified name as the registered name for the rr_selector 
%% gen_server.
%%
%%
%% Parameters: Name - an atom that is the registered name of the 
%% rr_selector gen_server to be used.
%%
%% Value: a valid Erlang type.
%%
%% @end
%%--------------------------------------------------------------------

-spec next(Name) -> term().
next()-> gen_server:call(Name, next_id).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init(Identifiers) ->
        {ok,Identifiers}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
handle_call(next_id, _From, Identifiers) ->
        {reply,to_do,[]};
handle_call(stop, _From, _State) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================


%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
%% Only include the eunit testing libary
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%%
%% This is where I have chosen to put the tests for this 
%% module. They could be moved into a separate file if 
%% that made more sense. Also, I have chosen to mock,
%% using meck, two worker modules. It would be possible to 
%% write all of the following tests without any mocking.
%% I have chosen to mock the two worker modules to 
%%

%% 
%% This type of test is referred to as a 'simple test.'
%% It is making sure that the empty worker clause of 
%% the start function is tested.
%%
empty_worker_list_start_test()->
    ?assertMatch({error,empty_worker_list},rr_balancer:start(some_id,[])).

%%
%% This test makes sure the second clause of the start 
%% function is properly called and is working as expected.
%%
filled_worker_list_start_test_()->
    {setup,
     % no setup function needed for this test

     % cleanup function
     % unload the dynamically created rr_selector
     fun(_) -> %% tell tster to stop itself and wait for it to be gone
               gen_statem:stop(some_id)
               end,
     [?_assertMatch({ok,_},rr_balancer:start(some_id,[a]))]}.

%%
%% Test to see if the worker list is dealt with correctly by
%% handle_event.
%%
handle_event_test_() ->
    [?_assertMatch({reply,sue,[bob,sally,grace,sue]},
        rr_balancer:handle_call(next_id, some_from, [sue,bob,sally,grace])),
     %test if list is empty
     ?_assertMatch({reply,empty_list_error,[]},
        rr_balancer:handle_call(next_id, some_from, [])),
     %test if state is not a list
     ?_assertMatch({reply,non_list_error,[]},
        rr_balancer:handle_call(next_id, some_from, true))

    ].



-endif.
