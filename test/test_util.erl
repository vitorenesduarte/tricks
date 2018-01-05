%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Vitor Enes. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(test_util).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/0,
         stop/0,
         event_subscribe/3,
         event_register/2,
         event_expect/2,
         event_expect/3,
         example_run/1]).

%% @doc Start app.
start() ->
    start_erlang_distribution(),

    Config = [{monitor_master, true},
              {boot_timeout, 10},
              {startup_functions, [{code, set_path, [codepath()]}]}],

    %% start node
    Node = case ct_slave:start(?APP, Config) of
        {ok, N} ->
            N;
        Error ->
            ct:fail(Error)
    end,

    %% load tricks 
    ok = rpc:call(Node, application, load, [?APP]),

    %% set lager log dir
    NodeDir = filename:join([home_dir(),
                             ".lager",
                             Node]),
    ok = rpc:call(Node,
                  application,
                  set_env,
                  [lager, log_root, NodeDir]),

    %% ensure started
    {ok, _} = rpc:call(Node,
                       application,
                       ensure_all_started,
                       [?APP]),

    put(node, Node),
    ok.

%% @doc Stop app.
stop() ->
    case ct_slave:stop(?APP) of
        {ok, _} ->
            ok;
        Error ->
            ct:fail(Error)
    end.

%% @doc Subscribe to an event.
event_subscribe(ExpId, Event0, Pid) ->
    Event = tricks_util:parse_event(Event0),
    ok = rpc:call(get(node),
                  tricks_event_manager,
                  subscribe,
                  [ExpId, Event, Pid]).

%% @doc Register an event.
event_register(ExpId, EventName0) ->
    EventName = tricks_util:parse_binary(EventName0),
    ok = rpc:call(get(node),
                  tricks_event_manager,
                  register,
                  [ExpId, EventName]).

%% @doc Expect an event.
%%      Fail if it does not meet expectations after 1s.
event_expect(ExpId, Event) ->
    event_expect(ExpId, Event, 1).

%% @doc Expect an event.
%%      Fail if it does not meet expectations after `Wait`s.
event_expect(ExpId, Event0, Wait) ->
    Event = tricks_util:parse_event(Event0),
    receive
        {notification, ExpId, Event} ->
            ok;
        {notification, A, B} ->
            ct:fail("Wrong event [~p] ~p", [A, B])
    after
        Wait * 1000 ->
            ct:fail("No event")
    end.

%% @doc Run an example.
example_run(Name) ->
    {ok, ExpId} = rpc:call(get(node),
                           tricks_example,
                           run,
                           [home_dir(), Name]),
    ExpId.

%% @private Start erlang distribution.
start_erlang_distribution() ->
    os:cmd(os:find_executable("epmd") ++ " -daemon"),

    {ok, Hostname} = inet:gethostname(),
    Runner = list_to_atom("runner@" ++ Hostname),

    case net_kernel:start([Runner, shortnames]) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.

%% @private
codepath() ->
    lists:filter(fun filelib:is_dir/1, code:get_path()).

%% @private
home_dir() ->
    os:getenv("TRICKS_HOME", "~/tricks").
