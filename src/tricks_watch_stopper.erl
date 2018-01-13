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

-module(tricks_watch_stopper).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).

-record(state, {}).


-spec start_link() -> {ok, pid()} | ignore | error().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop a pod watch.
-spec stop(pid()) -> ok | error().
stop(Pid) ->
    gen_server:cast(?MODULE, {stop, Pid}).

init([]) ->
    lager:info("tricks watch stopper initialized!"),

    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    {stop, {unhandled, Msg}, State}.

handle_cast({stop, Pid}, State) ->
    gen_statem:stop(Pid),
    {noreply, State}.
