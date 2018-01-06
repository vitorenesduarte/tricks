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

-module(tricks_event_manager_SUITE).
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
    [event_test].

%% ===================================================================
%% tests
%% ===================================================================

event_test(_Config) ->
    %% start
    ok = test_util:start(),

    %% check experiments ids are used properly
    test_util:event_subscribe(1, {event, 1}),
    test_util:event_subscribe(1, {event, 2}),
    test_util:event_subscribe(2, {event, 1}),

    test_util:event_register(1, event),
    test_util:event_expect(1, {event, 1}),

    test_util:event_register(2, event),
    test_util:event_expect(2, {event, 1}),

    test_util:event_register(1, event),
    test_util:event_expect(1, {event, 2}),

    %% subscription to an event that has already
    %% ocurred should also be notified
    test_util:event_subscribe(1, {event, 2}),
    test_util:event_subscribe(1, {event, 1}),

    test_util:event_expect(1, {event, 2}),
    test_util:event_expect(1, {event, 1}),

    %% stop
    ok = test_util:stop().
