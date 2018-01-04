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

-module(cal_scheduler).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("cal.hrl").

-behaviour(gen_server).

-define(CTX, ctx:background()).
-define(NAMESPACE, <<"default">>).

%% API
-export([start_link/0,
         schedule_pod/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).

-record(state, {kuberl_cfg :: maps:map(),
                pod_schedule :: orddict:orddict()}).

-type state_t() :: #state{}.

-spec start_link() -> {ok, pid()} | ignore | error().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Schedule pod, given its kuberl pod body
%%      and workflow start and stop information.
-spec schedule_pod(maps:map(), now | event(), never | event()) ->
    ok | error().
schedule_pod(Body, Start, Stop) ->
    gen_server:call(?MODULE, {schedule_pod, Body, Start, Stop}, infinity).

init([]) ->
    lager:info("cal scheduler initialized!"),

    %% init kuberl
    %Cfg = kuberl:cfg_with_host("kubernetes.default"),
    Cfg = #{},

    {ok, #state{kuberl_cfg=Cfg,
                pod_schedule=orddict:new()}}.

handle_call({schedule_pod, Body, Start, Stop}, _From, State0) ->
    %% schedule start
    State1 = case Start of
        now ->
            run_pod(Body, State0);
        _ ->
            add_pod_to_schedule(start, Start, Body, State0)
    end,

    %% schedule stop
    State2 = case Stop of
        never ->
            State1;
        _ ->
            add_pod_to_schedule(stop, Stop, Body, State1)
    end,

    {reply, ok, State2}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec run_pod(maps:map(), state_t()) -> state_t().
run_pod(Body, #state{kuberl_cfg=Cfg}=State) ->
    Result = kuberl_core_v1_api:create_namespaced_pod(
        ?CTX,
        ?NAMESPACE,
        Body,
        #{cfg => Cfg}
    ),

    case Result of
        {ok, _, _ResponseInfo} ->
            cal_pod_watch:watch(Body, Cfg);
        _ ->
            lager:info("Error creating pod ~p", [Result])
    end,

    State.

%% @private add start or pod stop to schedule
%% TODO what if event is already true?
-spec add_pod_to_schedule(start | stop, event(), map:map(), state_t()) ->
    state_t().
add_pod_to_schedule(What, Event, Body, #state{pod_schedule=Schedule0}=State) ->
    Schedule1 = orddict:append(Event, {What, Body}, Schedule0),
    State#state{pod_schedule=Schedule1}.
