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

-module(tricks_sup).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type, Timeout),
        {I, {I, start_link, []}, permanent, Timeout, Type, [I]}).
-define(CHILD(I), ?CHILD(I, worker, 5000)).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% configure
    configure(),

    %% start driver tcp acceptor
    start_driver_acceptor(),

    %% start http dispatch
    start_http_dispatch(),

    %% start app, scheduler, event manager,
    %% discovery manager
    Actors = [?APP,
              tricks_scheduler,
              tricks_event_manager,
              tricks_discovery_manager],
    Children = [?CHILD(A) || A <- Actors],

    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
configure() ->
    %% configure driver IP
    configure_from_env(string, "POD_IP", tricks_driver_ip),

    %% select random driver port
    tricks_config:set(tricks_driver_port, random_port()),

    %% configure k8s api server and token
    configure_from_env(string, "K8S_API_SERVER", k8s_api_server),
    configure_from_env(string, "K8S_API_TOKEN", k8s_api_token).

%% @private
start_driver_acceptor() ->
    Listener = tricks_driver_listener,
    Transport = ranch_tcp,
    %% TODO make this configurable
    Options = [{port, tricks_config:get(tricks_driver_port)},
               {max_connections, 1024},
               {num_acceptors, 1}],
    DriverHandler = tricks_driver_handler,

    {ok, _} = ranch:start_listener(Listener,
                                   Transport,
                                   Options,
                                   DriverHandler,
                                   []).

%% @private
start_http_dispatch() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/exp", tricks_http_exp_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        ?WEB_CONFIG,
        #{env => #{dispatch => Dispatch}}
    ).

%% @private From partisan code.
random_port() ->
    {ok, Socket} = gen_tcp:listen(0, []),
    {ok, {_, Port}} = inet:sockname(Socket),
    ok = gen_tcp:close(Socket),
    Port.

%% @private
configure_from_env(string, Env, Var) ->
    case os:getenv(Env) of
        false ->
            ok;
        Value ->
            tricks_config:set(Var, Value)
    end.
