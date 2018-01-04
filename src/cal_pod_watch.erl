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
    {ok, []}.

handle_event(error, #{message := Message}, State) ->
    lager:info("Error : ~p~n", [Message]),
    {ok, State};
handle_event(Type, #{metadata := #{labels := Labels}}, State) ->
    %% extract exp id and tag pod info
    %% from its labels
    #{expId := ExpId,
      tag   := Tag} = Labels,

    EventName = event_name(Type, Tag),

    %% if a valid event,
    %% register it in event manager
    case EventName of
        undefined ->
            ok;
        _ ->
            cal_event_manager:register(ExpId, EventName)
    end,

    %% if event type from kuberl
    %% is deleted, stop watching
    case Type of
        deleted ->
            %% TODO proper watch stop
            WatchPid = self(),
            spawn(fun() -> gen_statem:stop(WatchPid) end);
        _ ->
            ok
    end,

    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% @private Create event name given the event type from kuberl watch
%%          and the pod tag.
%%          If the event type is added,
%%          the event name is tag_start.
%%          If the event type is deleted,
%%          the event name is tag_stop.
event_name(added, Tag) when is_binary(Tag) ->
    <<Tag/binary, "_start">>;
event_name(deleted, Tag) when is_binary(Tag) ->
    <<Tag/binary, "_stop">>;
event_name(modified, Tag) when is_binary(Tag) ->
    undefined.
