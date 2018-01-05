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

-module(tricks_scheduler).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         schedule_pod/4]).

%% gen_server trickslbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-type schedule() :: dict:dict({exp_id(), event()}, {start | stop, maps:map()}).

-record(state, {kuberl_cfg :: maps:map(),
                schedule :: schedule()}).

-type state_t() :: #state{}.

-spec start_link() -> {ok, pid()} | ignore | error().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Schedule pod, given its kuberl pod body
%%      and workflow start and stop information.
-spec schedule_pod(exp_id(), maps:map(), now | event(), never | event()) ->
    ok | error().
schedule_pod(ExpId, Body, Start, Stop) ->
    gen_server:call(?MODULE,
                    {schedule_pod, ExpId, Body, Start, Stop},
                    infinity).

init([]) ->
    lager:info("tricks scheduler initialized!"),

    %% init kuberl
    %Cfg = kuberl:cfg_with_host("kubernetes.default"),
    Cfg = #{},

    {ok, #state{kuberl_cfg=Cfg,
                schedule=dict:new()}}.

handle_call({schedule_pod, ExpId, Body, Start, Stop}, _From, State0) ->
    %% schedule start
    State1 = case Start of
        now ->
            ok = start_pod(Body, State0),
            State0;
        _ ->
            add_pod_to_schedule(start, ExpId, Body, Start, State0)
    end,

    %% schedule stop
    State2 = case Stop of
        never ->
            State1;
        _ ->
            add_pod_to_schedule(stop, ExpId, Body, Stop, State1)
    end,

    {reply, ok, State2}.

handle_cast(Msg, State) ->
    {stop, {unhandled, Msg}, State}.

handle_info({notification, ExpId, Event}, #state{schedule=Schedule}=State0) ->
    lager:info("Notification [~p] ~p", [ExpId, Event]),

    %% TODO remove from schedule
    List = dict:fetch({ExpId, Event}, Schedule),
    lists:foreach(
        fun({What, Body}) ->
            case What of
                start ->
                    start_pod(Body, State0);
                stop ->
                    stop_pod(Body, State0)
            end
        end,
        List
    ),

    {noreply, State0}.

%% @private
-spec start_pod(maps:map(), state_t()) -> ok.
start_pod(Body, #state{kuberl_cfg=Cfg}) ->
    Result = kuberl_core_v1_api:create_namespaced_pod(
        ?CTX,
        ?NAMESPACE,
        Body,
        #{cfg => Cfg}
    ),

    case Result of
        {ok, _, _ResponseInfo} ->
            tricks_pod_watch:watch(Body, Cfg);
        _ ->
            lager:info("Error starting pod ~p", [Result])
    end,

    ok.

%% @private
-spec stop_pod(maps:map(), state_t()) -> ok.
stop_pod(#{metadata := #{name := PodName}}=_Body,
         #state{kuberl_cfg=Cfg}) ->
    Body = #{},
    Optional = #{params => #{gracePeriodSeconds => 5},
                 cfg => Cfg},
    Result = kuberl_core_v1_api:delete_namespaced_pod(
        ?CTX,
        PodName,
        ?NAMESPACE,
        Body,
        Optional
    ),

    case Result of
        {ok, _, _ResponseInfo} ->
            ok;
        _ ->
            lager:info("Error stopping pod", [Result])
    end.

%% @private add start or pod stop to schedule
%% TODO what if event is already true?
-spec add_pod_to_schedule(start | stop, exp_id(), map:map(), event(),
                          state_t()) -> state_t().
add_pod_to_schedule(What, ExpId, Body, Event,
                    #state{schedule=Schedule0}=State) ->
    tricks_event_manager:subscribe(ExpId, Event, self()),
    Schedule1 = dict:append({ExpId, Event}, {What, Body}, Schedule0),
    State#state{schedule=Schedule1}.
