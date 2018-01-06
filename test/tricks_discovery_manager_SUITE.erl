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
    Config.

end_per_testcase(Case, Config) ->
    ct:pal("Ending test case: ~p", [Case]),
    Config.

all() ->
    [discover_test].

%% ===================================================================
%% tests
%% ===================================================================

discover_test(_Config) ->
    %% start
    ok = test_util:start(),

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
    test_util:discovery_expect(ExpId, app1, [1], 60),
    test_util:discovery_expect(ExpId, app2, []),

    %% start app2
    test_util:event_register(ExpId, go2),
    test_util:discovery_expect(ExpId, app2, [1, 2, 3], 60),
    
    %% app1 should stop soon
    test_util:discovery_expect(ExpId, app1, [], 120),

    %% stop
    ok = test_util:stop().
