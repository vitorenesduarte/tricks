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

-module(tricks_discovery_manager).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         register/3,
         unregister/3,
         discover/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).

-type from() :: {pid(), term()}.
-type exp_data() :: #{pods => dict:dict(tag(),
                                        sets:set(pod_data())),
                      subs => disct:dict({tag(), integer()},
                                         [from()])}.

-define(EMPTY_POD_DATA, sets:new()).
-define(EMPTY_EXP_DATA, #{pods => dict:new(),
                          subs => dict:new()}).

-record(state, {exp_to_data :: dict:dict(exp_id(), exp_data())}).


-spec start_link() -> {ok, pid()} | ignore | error().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register pod.
-spec register(exp_id(), tag(), pod_data()) -> ok | error().
register(ExpId, Tag, {Id, Ip}=Data)
  when is_binary(ExpId), is_binary(Tag),
       is_integer(Id), is_binary(Ip) ->
    gen_server:cast(?MODULE, {register, ExpId, Tag, Data}).

%% @doc Unregister pod.
-spec unregister(exp_id(), tag(), pod_data()) -> ok | error().
unregister(ExpId, Tag, {Id, Ip}=Data)
  when is_binary(ExpId), is_binary(Tag),
       is_integer(Id), is_binary(Ip) ->
    gen_server:cast(?MODULE, {unregister, ExpId, Tag, Data}).

%% @doc Find at least `Min' pods in a given experiment `ExpId',
%%      with a given `Tag'.
-spec discover(exp_id(), tag(), integer()) -> {ok, [pod_data()]} | error().
discover(ExpId, Tag, Min)
  when is_binary(ExpId), is_binary(Tag) , is_integer(Min) ->
    gen_server:call(?MODULE, {discover, ExpId, Tag, Min}, infinity).

init([]) ->
    lager:info("tricks discover manager initialized!"),

    {ok, #state{exp_to_data=dict:new()}}.

handle_call({discover, ExpId, Tag, Min}, From,
            #state{exp_to_data=ETD0}=State) ->

    D0 = tricks_util:dict_find(ExpId, ETD0, ?EMPTY_EXP_DATA),
    #{pods := Pods,
      subs := Subs0} = D0,

    Set = tricks_util:dict_find(Tag, Pods, ?EMPTY_POD_DATA),

    case sets:size(Set) >= Min of
        true ->
            %% if enough pods,
            %% reply
            {reply, prepare_reply(Set), State};
        false ->
            %% otherwise, subscribe
            Subs1 = dict:append({Tag, Min}, From, Subs0),
            %% update subs
            D1 = D0#{subs => Subs1},
            ETD1 = dict:store(ExpId, D1, ETD0),
            {noreply, State#state{exp_to_data=ETD1}}
    end.

handle_cast({register, ExpId, Tag, Data},
            #state{exp_to_data=ETD0}=State) ->
    lager:info("Register pod [~p] ~p ~p", [ExpId, Tag, Data]),

    D0 = tricks_util:dict_find(ExpId, ETD0, ?EMPTY_EXP_DATA),
    #{pods := Pods0,
      subs := Subs0} = D0,

    %% add pod
    Set0 = tricks_util:dict_find(Tag, Pods0, ?EMPTY_POD_DATA),
    Set1 = sets:add_element(Data, Set0),

    %% update tag
    Pods1 = dict:store(Tag, Set1, Pods0),

    %% check if there's any subscription
    Key = {Tag, sets:size(Set1)},
    Subs1 = case dict:find(Key, Subs0) of
        {ok, Froms} ->
            %% reply to all
            [gen_server:reply(From, prepare_reply(Set1)) || From <- Froms],
            %% and erase subscriptions
            dict:erase(Key, Subs0);
        error ->
            %% otherwise, do nothing
            Subs0
    end,

    %% update experiment
    D1 = D0#{pods => Pods1,
             subs => Subs1},
    ETD1 = dict:store(ExpId, D1, ETD0),
    {noreply, State#state{exp_to_data=ETD1}};

handle_cast({unregister, ExpId, Tag, Data},
            #state{exp_to_data=ETD0}=State) ->
    lager:info("Unregister pod [~p] ~p ~p", [ExpId, Tag, Data]),

    D0 = tricks_util:dict_find(ExpId, ETD0, ?EMPTY_EXP_DATA),
    #{pods := Pods0} = D0,

    %% remove pod
    Set0 = tricks_util:dict_find(Tag, Pods0, ?EMPTY_POD_DATA),
    Set1 = sets:del_element(Data, Set0),

    Pods1 = case sets:size(Set1) of
        0 ->
            %% remove tag if no pods
            dict:erase(Tag, Pods0);
        _ ->
            %% otherwise update
            dict:store(Tag, Set1, Pods0)
    end,

    ETD1 = case dict:size(Pods1) of
        0 ->
            %% remove experiment if no tags
            dict:erase(ExpId, ETD0);
        _ ->
            %% otherwise update
            D1 = D0#{pods => Pods1},
            dict:store(ExpId, D1, ETD0)
    end,
    {noreply, State#state{exp_to_data=ETD1}}.


%% @private
prepare_reply(Set) ->
    {ok, sets:to_list(Set)}.
