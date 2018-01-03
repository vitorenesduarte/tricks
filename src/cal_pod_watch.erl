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
-spec watch(maps:map(), maps:map()) -> ok.
watch(Body, Cfg) ->
    Optional = #{params => #{labelSelector => cal_exp:label_selector(Body)},
                 cfg => Cfg},
    kuberl_watch:start_link(?MODULE,
                            kuberl_core_v1_api,
                            list_namespaced_pod,
                            [ctx:background(), <<"default">>],
                            Optional,
                            []).

init([]) ->
    {ok, []}.

handle_event(added, #{metadata := #{name := Name}}, State) ->
    io:format("Added : ~p~n", [Name]),
    {ok, State};
handle_event(deleted, #{metadata := #{name := Name}}, State) ->
    io:format("Deleted : ~p~n", [Name]),
    {ok, State};
handle_event(modified, #{metadata := #{name := Name}}, State) ->
    io:format("Modified : ~p~n", [Name]),
    {ok, State};
handle_event(error, #{message := Message}, State) ->
    io:format("Error : ~p~n", [Message]),
    {ok, State}.

terminate(Reason, _State) ->
    io:format("Terminating : ~p~n", [Reason]),
    ok.
