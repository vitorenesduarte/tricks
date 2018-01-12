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

-module(tricks_exp).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

%% API
-export([exp_id/0,
         pod_body/3,
         label_selector/1]).

-define(SEP, <<"-">>).

%% @doc Generate experiment identifier.
-spec exp_id() -> exp_id().
exp_id() ->
    tricks_util:parse_binary(
      erlang:system_time(millisecond)
    ).

%% @doc Body for Kubernetes pod creation.
-spec pod_body(exp_id(), integer(), maps:map()) ->
    maps:map().
pod_body(ExpId, PodId0, #{tag := Tag}=EntrySpec0) ->
    PodId = tricks_util:parse_binary(PodId0),

    %% create pod name
    PodName = pod_name(Tag, ExpId, PodId),

    %% update entry spec
    EntrySpec = EntrySpec0#{expId => ExpId,
                            podId => PodId,
                            name => PodName},

    %% create pod metadata
    Metadata = metadata(EntrySpec),

    %% create pod spec
    Spec = spec(EntrySpec),

    %% create request body
    #{apiVersion => <<"v1">>,
      kind => <<"Pod">>,
      metadata => Metadata,
      spec => Spec}.

%% @doc Compute pod label selector.
-spec label_selector(maps:map()) -> binary().
label_selector(#{metadata := #{labels := Labels}}=_PodBody) ->
    Selectors = maps:fold(
        fun(Label, Value, Acc) ->
            [tricks_util:binary_join(<<"=">>, [Label, Value]) | Acc]
        end,
        [],
        Labels
    ),
    tricks_util:binary_join(<<",">>, Selectors).

%% @private Generate pod name.
%%          Given it's entry name/tag (x), experiment id (y), and pod (z),
%%          and a separator -,
%%          the pod name will be x-y-z
-spec pod_name(binary(), binary(), binary()) -> binary().
pod_name(Tag, ExpId, PodId) ->
    <<Tag/binary, ?SEP/binary, ExpId/binary, ?SEP/binary, PodId/binary>>.

%% @private Generate pod metadata.
-spec metadata(maps:map()) -> maps:map().
metadata(#{tag := Tag,
           expId := ExpId,
           podId := PodId,
           name := PodName}) ->
    #{name => PodName,
      labels => #{tag => Tag,
                  expId => ExpId,
                  podId => PodId}}.

%% @private Generate pod spec.
-spec spec(map:map()) -> maps:map().
spec(#{name  := PodName,
       image := Image}=EntrySpec) ->
    #{restartPolicy => <<"Never">>,
      containers => [#{name => PodName,
                       image => Image,
                       imagePullPolicy => <<"Always">>,
                       env => env(EntrySpec)}]}.

%% @private Append env vars:
%%   - TAG
%%   - REPLICAS
%%   - EXP_ID
%%   - POD_ID
%%   - POD_IP
%%   - TRICKS_IP
%%   - TRICKS_PORT
-spec env(maps:map()) -> maps:map().
env(#{tag := Tag,
      replicas := Replicas,
      expId := ExpId,
      podId := PodId,
      env := Env0}) ->

    %% Get tricks driver IP and port
    TricksIp = tricks_config:get(tricks_driver_ip,
                                 "localhost"),
    TricksPort = tricks_config:get(tricks_driver_port),

    Env = [#{name => <<"TAG">>,
             value => Tag},
           #{name => <<"REPLICAS">>,
             value => Replicas},
           #{name => <<"EXP_ID">>,
             value => ExpId},
           #{name => <<"POD_ID">>,
             value => PodId},
           #{name => <<"POD_IP">>,
             valueFrom => #{fieldRef
                            => #{fieldPath
                                 => <<"status.podIP">>}}
            },
           #{name => <<"TRICKS_IP">>,
             value => TricksIp},
           #{name => <<"TRICKS_PORT">>,
             value => TricksPort} | Env0],
    parse_env(Env).

%% @private Parse env, converting integer values to binary.
-spec parse_env(maps:map()) -> maps:map().
parse_env([]) ->
    [];
parse_env([H0|T]) ->
    H = maps:map(
        fun(K, V) ->
            case K of
                valueFrom -> V;
                _ -> tricks_util:parse_binary(V)
            end
        end,
        H0
    ),
    [H | parse_env(T)].
