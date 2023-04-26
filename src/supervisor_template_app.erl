%%%-------------------------------------------------------------------
%% @doc supervisor_template public API
%% @end
%%%-------------------------------------------------------------------

-module(supervisor_template_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    supervisor_template_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
