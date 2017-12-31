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

-module(cal).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("cal.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% TODO remove
-export([example/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).

-record(state, {}).

-spec start_link() -> {ok, pid()} | ignore | error().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run an experiment.
-spec example() -> ok | error().
example() ->
    gen_server:call(?MODULE, {run, exp()}, infinity).

init([]) ->
    lager:info("cal initialized!"),

    %% init kuberl
    %application:set_env(kuberl, host, "kubernetes.default"),

    {ok, #state{}}.

handle_call({run, Experiment}, _From, State) ->
    run(Experiment),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

run(Experiment) ->
    #{<<"experiment">> := CEs} = Experiment,

    lists:foreach(
        fun(CE0) ->
            %% extract number of replicas of this CE
            #{<<"replicas">> := Replicas} = CE0,

            lists:foreach(
                fun(Id) ->
                    %% append id to pod info
                    CE = CE0#{<<"id">> => id(Id)},

                    %% create pod metadata
                    Metadata = metadata(CE),

                    %% create pod spec
                    #{<<"name">> := Name} = Metadata,
                    Spec = spec(Name, CE),

                    %% create request body
                    Body = #{<<"apiVersion">> => <<"v1">>,
                             <<"kind">> => <<"Pod">>,
                             <<"metadata">> => Metadata,
                             <<"spec">> => Spec},

                    %% create pod
                    Ctx = undefined,
                    Namespace = "default",
                    %% TODO
                    %% replace by kuberl_core_v1_api:create_namespaced_pod
                    R = kuberl_utils:request(
                        Ctx,
                        post,
                        ["/api/v1/namespaces/", Namespace, "/pods"],
                        [],
                        [{<<"Content-Type">>, <<"application/json">>}],
                        Body,
                        []
                    ),
                    lager:info("Response ~p", [R])

                end,
                lists:seq(1, Replicas)
            )
        end,
        CEs
    ).

-define(SEP, <<"-">>).

-spec id(integer()) -> binary().
id(Id) ->
    integer_to_binary(Id).

-spec metadata(maps:map()) -> maps:map().
metadata(#{<<"tag">> := Tag, <<"id">> := Id}) ->
    #{<<"name">> => name(Tag, Id),
      <<"labels">> => #{<<"tag">> => Tag,
                        <<"id">> => Id}}.

-spec name(binary(), binary()) -> binary().
name(Tag, Id) ->
    <<Tag/binary, ?SEP/binary, Id/binary>>.

-spec spec(binary(), maps:map()) -> maps:map().
spec(Name, #{<<"image">> := Image}=CE) ->
    #{<<"restartPolicy">> => <<"Never">>,
      <<"containers">> => [#{<<"name">> => Name,
                             <<"image">> => Image,
                             <<"imagePullPolicy">> => <<"Always">>,
                             <<"env">> => env(CE)}]}.

%% @doc Append env vars:
%%   - ID
%%   - IP
-spec env(maps:map()) -> maps:map().
env(#{<<"id">> := Id, <<"env">> := Env}) ->
    [#{<<"name">> => <<"ID">>,
       <<"value">> => Id},
     #{<<"name">> => <<"IP">>,
       <<"valueFrom">> => #{<<"fieldRef">>
                            => #{<<"fieldPath">>
                                 => <<"status.podIP">>}}
      } | Env].

exp() ->
    #{<<"apiVersion">> => <<"v1">>,
      <<"experiment">> =>
      [#{<<"tag">> => <<"hello-world">>,
         <<"image">> => <<"vitorenesduarte/cal-example">>,
         <<"replicas">> => 1,
         <<"env">> =>
         [#{<<"name">> => <<"TYPE">>,
            <<"value">> => <<"hello-world">>},
          #{<<"name">> => <<"COUNT">>,
            <<"value">> => <<"10">>}]}]}.
