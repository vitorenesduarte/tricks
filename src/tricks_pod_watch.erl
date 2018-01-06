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

-module(tricks_pod_watch).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

%% API
-export([watch/2]).

%% kuberl watch
-export([init/1,
         handle_event/3,
         terminate/2]).

-define(PENDING, 0).
-define(RUNNING, 1).
-define(STOPPED, 2).
-define(UNKNOWN, 3).

-type pod_id() :: integer().
-type pod_ip() :: list().

-record(state, {current :: ?PENDING | ?RUNNING | ?STOPPED,
                pod_data :: {pod_id(), pod_ip()} | undefined}).

%% @doc Watch changes on pod.
%%      It takes as an argument
%%      kuberl pod body.
-spec watch(maps:map(), maps:map()) -> {ok, pid()} | ignore | error().
watch(Body, Cfg) ->
    Optional = #{params => #{labelSelector
                             => tricks_exp:label_selector(Body)},
                 cfg => Cfg},
    kuberl_watch:start_link(?MODULE,
                            kuberl_core_v1_api,
                            list_namespaced_pod,
                            [?CTX, ?NAMESPACE],
                            Optional,
                            []).

init([]) ->
    {ok, #state{current=?PENDING,
                pod_data=undefined}}.

handle_event(error, #{message := Message}, State) ->
    lager:info("Error : ~p~n", [Message]),
    {ok, State};
handle_event(Type, #{metadata := #{labels := Labels},
                     status   := #{phase := Phase}=Status}, State0) ->
    %% extract exp id and tag pod info
    %% from its labels
    #{expId := ExpId0,
      podId := PodId0,
      tag   := Tag} = Labels,
    ExpId = tricks_util:parse_integer(ExpId0),
    PodId = tricks_util:parse_integer(PodId0),

    PodIp = case maps:find(podIP, Status) of
        {ok, V} ->
            tricks_util:parse_list(V);
        error ->
            undefined
    end,

    %% create pod data
    Data = {PodId, PodIp},

    PodStatus = parse_pod_status(Type, Phase),
    %% get diff between previous status
    %% and current status
    {Diff, State1} = diff(PodStatus, State0),

    %% register events
    register_events(ExpId, Tag, Diff),

    %% register discovery
    State2 = register_discovery(ExpId, Tag, Data, Diff, State1),

    %% if pod is stopped,
    %% TODO stop watching
    case State2 of
        #state{current=?STOPPED} ->
            %WatchPid = self(),
            %spawn(fun() -> gen_statem:stop(WatchPid) end);
            ok;
        _ ->
            ok
    end,

    {ok, State2}.

terminate(_Reason, _State) ->
    ok.

%% @private A pod is stopped if it terminated
%%          or if it was deleted by us.
parse_pod_status(_, <<"Running">>) ->   ?RUNNING;
parse_pod_status(_, <<"Succeeded">>) -> ?STOPPED;
parse_pod_status(deleted, _) ->         ?STOPPED;
parse_pod_status(_, _) ->               ?UNKNOWN.

%% @private Get diff of status.
diff(?RUNNING, #state{current=Current}=State) ->
    %% if running,
    %% and our current is pending,
    %% return start
    Diff = case Current of
        ?PENDING -> [start];
        _ -> []
    end,
    {Diff, State#state{current=?RUNNING}};

diff(?STOPPED, #state{current=Current}=State) ->
    Diff = case Current of
        ?PENDING ->
            %% if succeeded and our current is pending,
            %% return start and stop
            [start, stop];
        ?RUNNING ->
            %% if our current is running
            %% only return stop
            [stop];
        ?STOPPED ->
            []
    end,
    {Diff, State#state{current=?STOPPED}};

diff(?UNKNOWN, State) ->
    {[], State}.

%% @private Register events given diff.
register_events(ExpId, Tag, Diff) ->
    [tricks_event_manager:register(ExpId, event_name(Kind, Tag)) ||
     Kind <- Diff].

%% @private Compute event name,
event_name(start, Tag) ->
    tricks_util:binary_join([Tag, <<"_start">>]);
event_name(stop, Tag) ->
    tricks_util:binary_join([Tag, <<"_stop">>]).

%% @private Register discovery in case
%%          single diff
%%          (i.e. only start, or only stop).
register_discovery(_, _, _, [], State) ->
    State;
register_discovery(ExpId, Tag, Data, [start], State) ->
    tricks_discovery_manager:register(ExpId, Tag, Data),
    %% store pod data to be used when unregistering
    State#state{pod_data=Data};
register_discovery(ExpId, Tag, _, [stop], #state{pod_data=Data}=State) ->
    %% use stored pod data for unregistering
    tricks_discovery_manager:unregister(ExpId, Tag, Data),
    State;
register_discovery(_, _, _, [start, stop], State) ->
    State.
