%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Vitor Enes.  All Rights Reserved.
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

%% @doc Client socket message encoding.

-module(tricks_client_message).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([decode/1,
         encode/2]).

-type message() :: {event, event_name()}
                 | {subscription, event()}
                 | {notification, event()}
                 | {discovery, tag()}
                 | {pods, {tag(), [pod_data()]}}.

%% @doc Decode message.
-spec decode(binary()) -> maps:map().
decode(Bin) ->
    tricks_util:parse_json(Bin).

%% @doc Encode message.
-spec encode(exp_id(), message()) -> binary().
encode(ExpId, {Type, _}=Message) ->
    Base = #{expId => ExpId,
             type => Type},
    Payload = encode_payload(Message),
    Map = maps:merge(Base, Payload),

    tricks_util:compose_json(Map).

%% @private
-spec encode_payload(message()) -> maps:map().
encode_payload({event, EventName}) ->
    #{eventName => EventName};
encode_payload({subscription, {EventName, Value}}) ->
    #{eventName => EventName,
      value => Value};
encode_payload({notification, {EventName, Value}}) ->
    #{eventName => EventName,
      value => Value};
encode_payload({discovery, Tag}) ->
    #{tag => Tag};
encode_payload({pods, {Tag, PodsData}}) ->
    #{tag => Tag,
      pods => [#{id => Id,
                 ip => Ip} || {Id, Ip} <- PodsData]}.

%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).

event_test() ->
    ExpId = 17,
    EventName = <<"server_stop">>,

    Expected = #{expId => ExpId,
                 type => <<"event">>,
                 eventName => EventName},
    ?assertEqual(Expected, decode(encode(ExpId, {event, EventName}))).

subscription_test() ->
    ExpId = 17,
    EventName = <<"server_stop">>,
    Value = 5,
    Event = {EventName, Value},

    Expected = #{expId => ExpId,
                 type => <<"subscription">>,
                 eventName => EventName,
                 value => Value},
    ?assertEqual(Expected, decode(encode(ExpId, {subscription, Event}))).

notification_test() ->
    ExpId = 17,
    EventName = <<"server_stop">>,
    Value = 5,
    Event = {EventName, Value},

    Expected = #{expId => ExpId,
                 type => <<"notification">>,
                 eventName => EventName,
                 value => Value},
    ?assertEqual(Expected, decode(encode(ExpId, {notification, Event}))).

discovery_test() ->
    ExpId = 17,
    Tag = <<"server">>,
    Expected = #{expId => ExpId,
                 type => <<"discovery">>,
                 tag => Tag},
    ?assertEqual(Expected, decode(encode(ExpId, {discovery, Tag}))).

pods_test() ->
    ExpId = 17,
    Tag = <<"server">>,
    PodId1 = 1,
    PodIp1 = "127.0.0.1",
    PodId2 = 2,
    PodIp2 = "127.0.0.2",
    PodsData = [{PodId1, PodIp1},
                {PodId2, PodIp2}],

    Expected = #{expId => ExpId,
                 type => <<"pods">>,
                 tag => Tag,
                 pods => [#{id => PodId1, ip => PodIp1},
                          #{id => PodId2, ip => PodIp2}]},
    ?assertEqual(Expected, decode(encode(ExpId, {pods, {Tag, PodsData}}))).

-endif.
