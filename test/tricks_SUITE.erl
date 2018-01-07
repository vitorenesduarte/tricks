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

-module(tricks_SUITE).
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
    [hello_world_test,
     implicit_events_test].

%% ===================================================================
%% tests
%% ===================================================================

hello_world_test(_Config) ->
    ExpId = test_util:example_run("hello-world"),
    test_util:event_subscribe(ExpId, {"hello-world_start", 1}),
    test_util:event_subscribe(ExpId, {"hello-world_stop", 1}),

    %% wait for start
    test_util:event_expect(ExpId, {"hello-world_start", 1}, 20),
    %% wait for stop
    test_util:event_expect(ExpId, {"hello-world_stop", 1}, 60).

implicit_events_test(_Config) ->
    ExpId = test_util:example_run("implicit-events"),
    test_util:event_subscribe(ExpId, {"server2_stop", 5}),

    %% wait for end of 5 server2
    test_util:event_expect(ExpId, {"server2_stop", 5}, 300).
