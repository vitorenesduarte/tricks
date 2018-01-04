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

-module(cal_pod_watch).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("cal.hrl").

%% API
-export([watch/2]).

%% kuberl watch
-export([init/1,
         handle_event/3,
         terminate/2]).

-define(PENDING, 0).
-define(RUNNING, 1).
-define(STOPPED, 2).

-record(state, {current :: ?PENDING | ?RUNNING | ?STOPPED}).

%% @doc Watch changes on pod.
%%      It takes as an argument
%%      kuberl pod body.
-spec watch(maps:map(), maps:map()) -> {ok, pid()} | ignore | error().
watch(Body, Cfg) ->
    Optional = #{params => #{labelSelector => cal_exp:label_selector(Body)},
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
handle_event(_Type, #{metadata := #{labels := Labels},
                      status   := #{phase := Phase}}, State0) ->
    %% extract exp id and tag pod info
    %% from its labels
    #{expId := ExpId,
      tag   := Tag} = Labels,

    {Events, State1} = get_events(Phase, Tag, State0),

    case Events of
        undefined ->
            ok;
        _ ->
            %% register all events
            [cal_event_manager:register(ExpId, Event) ||
             Event <- Events]
    end,

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

%% @private Create events given pod phase.
get_events(<<"Running">>, Tag,
           #state{current=?PENDING}=State) when is_binary(Tag) ->
    %% if running,
    %% and our current is pending,
    %% return start event
    Events = [<<Tag/binary, "_start">>],
    {Events, State#state{current=?RUNNING}};

get_events(<<"Succeeded">>, Tag,
           #state{current=Current}=State) when is_binary(Tag) ->
    Events = case Current of
        ?PENDING ->
            %% if succeeded and our current is pending,
            %% return start and stop events
            [<<Tag/binary, "_start">>,
             <<Tag/binary, "_stop">>];
        ?RUNNING ->
            %% if our current is running
            %% only return stop event
            [<<Tag/binary, "_stop">>];
        ?STOPPED ->
            []
    end,
    {Events, State#state{current=?STOPPED}};

get_events(Phase, Tag, State) when is_binary(Tag) ->
    lager:info("NON EVENT ~p ~p", [Phase, Tag]),
    {undefined, State}.
