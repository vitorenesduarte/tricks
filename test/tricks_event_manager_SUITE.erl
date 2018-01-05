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

%% common_test trickslbacks
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
    Node = test_util:start(),
    put(node, Node),
    
    Receiver = self(),

    %% experiments ids are used properly
    event_subscribe(1, {event, 1}, Receiver),
    event_subscribe(1, {event, 2}, Receiver),
    event_subscribe(2, {event, 1}, Receiver),

    event_register(1, event),
    event_expect(1, {event, 1}),

    event_register(2, event),
    event_expect(2, {event, 1}),

    event_register(1, event),
    event_expect(1, {event, 2}),

    %% subscription to an event that has already
    %% ocurred should also be notified
    event_subscribe(1, {event, 2}, Receiver),
    event_subscribe(1, {event, 1}, Receiver),

    event_expect(1, {event, 2}),
    event_expect(1, {event, 1}),

    %% stop
    ok = test_util:stop().

%% @private
event_subscribe(ExpId, Event0, Pid) ->
    Event = tricks_util:parse_event(Event0),
    ok = rpc:call(get(node),
                  tricks_event_manager,
                  subscribe,
                  [ExpId, Event, Pid]).

%% @private
event_register(ExpId, EventName0) ->
    EventName = tricks_util:parse_binary(EventName0),
    ok = rpc:call(get(node),
                  tricks_event_manager,
                  register,
                  [ExpId, EventName]).

%% @private
event_expect(ExpId, Event0) ->
    Event = tricks_util:parse_event(Event0),
    receive
        {notification, ExpId, Event} ->
            ok;
        {notification, A, B} ->
            ct:fail("Wrong event [~p] ~p", [A, B])
    after
        1000 ->
            ct:fail("No event")
    end.
