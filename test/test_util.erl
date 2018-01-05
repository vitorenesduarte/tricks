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

-module(test_util).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("cal.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/0,
         stop/0]).

%% @doc Start app.
-spec start() -> atom().
start() ->
    start_erlang_distribution(),

    Config = [{monitor_master, true},
              {boot_timeout, 10},
              {startup_functions, [{code, set_path, [codepath()]}]}],

    %% start node
    Node = case ct_slave:start(?APP, Config) of
        {ok, N} ->
            N;
        Error ->
            ct:fail(Error)
    end,

    %% load cal
    ok = rpc:call(Node, application, load, [?APP]),

    %% set lager log dir
    NodeDir = filename:join([home_dir(),
                             ".lager",
                             Node]),
    ok = rpc:call(Node,
                  application,
                  set_env,
                  [lager, log_root, NodeDir]),

    %% ensure started
    {ok, _} = rpc:call(Node,
                       application,
                       ensure_all_started,
                       [?APP]),

    Node.

%% @doc Stop app.
-spec stop() -> ok.
stop() ->
    case ct_slave:stop(?APP) of
        {ok, _} ->
            ok;
        Error ->
            ct:fail(Error)
    end.

%% @private Start erlang distribution.
-spec start_erlang_distribution() -> ok.
start_erlang_distribution() ->
    os:cmd(os:find_executable("epmd") ++ " -daemon"),

    {ok, Hostname} = inet:gethostname(),
    Runner = list_to_atom("runner@" ++ Hostname),

    case net_kernel:start([Runner, shortnames]) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.

%% @private
-spec codepath() -> list(file:filename()).
codepath() ->
    lists:filter(fun filelib:is_dir/1, code:get_path()).

%% @private
home_dir() ->
    os:getenv("CAL_HOME", "~/cal").
