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

-module(tricks_discovery_manager_SUITE).
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

    Config.

end_per_testcase(Case, Config) ->
    ct:pal("Ending test case: ~p", [Case]),

    %% stop
    ok = test_util:stop(),

    Config.

all() ->
    [register_test,
     unregister_test,
     nada_test].

%% ===================================================================
%% tests
%% ===================================================================

register_test(_Config) ->
    %% there's nothing registered
    test_util:discovery_expect(10001, server, []),
    test_util:discovery_expect(10001, client, []),
    test_util:discovery_expect(10002, server, []),

    %% register and expect
    test_util:discovery_register(10001, server, {1, "127.0.0.1"}),
    test_util:discovery_expect(10001, server, [1]),
    test_util:discovery_expect(10001, client, []),
    test_util:discovery_expect(10002, server, []),

    %% register twice and expect
    test_util:discovery_register(10002, server, {10, "127.0.0.1"}),
    test_util:discovery_register(10001, server, {1, "127.0.0.1"}),
    test_util:discovery_register(10002, server, {10, "127.0.0.1"}),
    test_util:discovery_expect(10001, server, [1]),
    test_util:discovery_expect(10001, client, []),
    test_util:discovery_expect(10002, server, [10]),

    %% register and expect
    test_util:discovery_register(10001, server, {2, "127.0.0.2"}),
    test_util:discovery_expect(10001, server, [1, 2]),
    test_util:discovery_expect(10001, client, []),
    test_util:discovery_expect(10002, server, [10]),

    %% register and expect
    test_util:discovery_register(10001, client, {100, "127.0.0.100"}),
    test_util:discovery_expect(10001, server, [1, 2]),
    test_util:discovery_expect(10001, client, [100]),
    test_util:discovery_expect(10002, server, [10]).

unregister_test(_Config) ->
    %% unregister something non existing
    test_util:discovery_unregister(10001, server, {1, "127.0.0.1"}),

    %% register stuff
    test_util:discovery_register(10001, server, {1, "127.0.0.1"}),
    test_util:discovery_register(10001, server, {2, "127.0.0.2"}),
    test_util:discovery_register(10001, client, {100, "127.0.0.100"}),
    test_util:discovery_register(10002, server, {10, "127.0.0.1"}),

    %% unregister something non existing
    test_util:discovery_unregister(10001, server, {3, "127.0.0.3"}),
    test_util:discovery_unregister(10002, server, {3, "127.0.0.3"}),

    %% expect
    test_util:discovery_expect(10001, server, [1, 2]),
    test_util:discovery_expect(10001, client, [100]),
    test_util:discovery_expect(10002, server, [10]),

    %% unregister something existing
    test_util:discovery_unregister(10001, server, {2, "127.0.0.2"}),
    test_util:discovery_expect(10001, server, [1]),
    %% register it back
    test_util:discovery_register(10001, server, {2, "127.0.0.2"}),
    test_util:discovery_expect(10001, server, [1, 2]),
    %% unregister it again
    test_util:discovery_unregister(10001, server, {2, "127.0.0.2"}),
    test_util:discovery_expect(10001, server, [1]).

nada_test(_Config) ->
    %% start nada
    %% this experiment will
    %% - start 1 app1 when we register event go1
    %% - start 3 app2 when we register event go2
    %% - stop  1 app1 when 3 app2 are started
    ExpId = test_util:example_run("nada"),

    %% in the beginning there's nothing
    test_util:discovery_expect(ExpId, app1, []),
    test_util:discovery_expect(ExpId, app2, []),

    %% start app1
    test_util:event_register(ExpId, go1),
    test_util:discovery_expect(ExpId, app1, 1, [1]),
    test_util:discovery_expect(ExpId, app2, []),

    %% start app2
    test_util:event_register(ExpId, go2),
    test_util:discovery_expect(ExpId, app2, 3, [1, 2, 3]).
