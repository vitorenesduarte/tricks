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

-module(tricks_client_SUITE).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

%% common_test callbacks
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

-compile([nowarn_export_all, export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap, {hours, 1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(Case, Config) ->
    ct:pal("Beginning test case: ~p", [Case]),

    %% start
    ok = test_util:start(),

    %% connect to tricks
    ok = test_util:client_connect(),

    Config.

end_per_testcase(Case, Config) ->
    ct:pal("Ending test case: ~p", [Case]),

    %% disconnect from tricks
    ok = test_util:client_disconnect(),

    %% stop
    ok = test_util:stop(),

    Config.

all() ->
    [register_event_test,
     subscribe_event_test,
     discovery_test].

%% ===================================================================
%% tests
%% ===================================================================

register_event_test(_Config) ->
    %% subscribe to an event
    test_util:event_subscribe(17, {event, 1}),
    test_util:event_subscribe(17, {event, 2}),

    %% register an event using the client
    test_util:client_event_register(17, event),
    test_util:event_expect(17, {event, 1}),
    
    %% register another event
    test_util:client_event_register(17, event),
    test_util:event_expect(17, {event, 2}).

subscribe_event_test(_Config) ->
    %% subscribe to an event using the client
    test_util:client_event_subscribe(17, {event, 1}),
    test_util:client_event_subscribe(17, {event, 2}),

    %% register an event
    test_util:event_register(17, event),
    test_util:client_event_expect(17, {event, 1}),
    
    %% register another event
    test_util:event_register(17, event),
    test_util:client_event_expect(17, {event, 2}).

discovery_test(_Config) ->
    %%  expect nothing
    test_util:client_discovery_expect(17, server, []),

    %% register and expect
    test_util:discovery_register(17, server, {1, "127.0.0.1"}),
    test_util:client_discovery_expect(17, server, [1]),

    %% register and expect
    test_util:discovery_register(18, client, {2, "127.0.0.2"}),
    test_util:client_discovery_expect(18, client, [2]),
    test_util:discovery_register(18, client, {1, "127.0.0.12"}),
    test_util:client_discovery_expect(18, client, [1, 2]),

    %% unregister and expect
    test_util:discovery_unregister(18, client, {1, "127.0.0.12"}),
    test_util:client_discovery_expect(18, client, [2]),
    test_util:client_discovery_expect(17, server, [1]).
