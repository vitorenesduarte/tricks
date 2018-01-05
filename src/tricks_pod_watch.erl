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

-record(state, {current :: ?PENDING | ?RUNNING | ?STOPPED}).

%% @doc Watch changes on pod.
%%      It takes as an argument
%%      kuberl pod body.
-spec watch(maps:map(), maps:map()) -> {ok, pid()} | ignore | error().
watch(Body, Cfg) ->
    Optional = #{params => #{labelSelector => tricks_exp:label_selector(Body)},
                 cfg => Cfg},
    kuberl_watch:start_link(?MODULE,
                            kuberl_core_v1_api,
                            list_namespaced_pod,
                            [?CTX, ?NAMESPACE],
                            Optional,
                            []).

init([]) ->
    {ok, #state{current=?PENDING}}.

handle_event(error, #{message := Message}, State) ->
    lager:info("Error : ~p~n", [Message]),
    {ok, State};
handle_event(Type, #{metadata := #{labels := Labels},
                     status   := #{phase := Phase}}, State0) ->
    %% extract exp id and tag pod info
    %% from its labels
    #{expId := ExpId0,
      tag   := Tag} = Labels,
    ExpId = tricks_util:parse_integer(ExpId0),

    PodStatus = parse_pod_status(Type, Phase),
    {Events, State1} = case PodStatus of
        ?UNKNOWN ->
            lager:info("NON EVENT ~p ~p", [Type, Phase]),
            {[], State0};
        _ ->
            get_events(PodStatus, Tag, State0)
    end,

    %% register all events
    [tricks_event_manager:register(ExpId, Event) || Event <- Events],

    %% if pod is stopped,
    %% stop watching
    case State1 of
        #state{current=?STOPPED} ->
            %% TODO stop watch
            %WatchPid = self(),
            %spawn(fun() -> gen_statem:stop(WatchPid) end);
            ok;
        _ ->
            ok
    end,

    {ok, State1}.

terminate(_Reason, _State) ->
    ok.

%% @private A pod is stopped if it terminated
%%          or if it was deleted by us.
parse_pod_status(_Type, <<"Running">>) ->   ?RUNNING;
parse_pod_status(_Type, <<"Succeeded">>) -> ?STOPPED;
parse_pod_status(deleted, _Phase) ->        ?STOPPED;
parse_pod_status(_, _) ->                   ?UNKNOWN.

%% @private Create events given pod status.
get_events(?RUNNING, Tag, #state{current=Current}=State) ->
    %% if running,
    %% and our current is pending,
    %% return start event
    Events = case Current of
        ?PENDING ->
            [event(start, Tag)];
        _ ->
            []
    end,
    {Events, State#state{current=?RUNNING}};

get_events(?STOPPED, Tag, #state{current=Current}=State) ->
    Events = case Current of
        ?PENDING ->
            %% if succeeded and our current is pending,
            %% return start and stop events
            [event(start, Tag), event(stop, Tag)];
        ?RUNNING ->
            %% if our current is running
            %% only return stop event
            [event(stop, Tag)];
        ?STOPPED ->
            []
    end,
    {Events, State#state{current=?STOPPED}}.

%% @private
event(start, Tag) ->
    tricks_util:binary_join([Tag, <<"_start">>]);
event(stop, Tag) ->
    tricks_util:binary_join([Tag, <<"_stop">>]).
