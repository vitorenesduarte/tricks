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

-module(cal).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("cal.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         run/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).

-record(state, {}).

-spec start_link() -> {ok, pid()} | ignore | error().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run an experiment.
-spec run(maps:map()) -> ok | error().
run(Exp) ->
    gen_server:call(?MODULE, {run, Exp}, infinity).

init([]) ->
    lager:info("cal initialized!"),
    {ok, #state{}}.

handle_call({run, Experiment}, _From, State) ->
    do_run(Experiment),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private Run an experiment given its config and kuberl config.
do_run(Experiment) ->
    #{<<"experiment">> := EntrySpecs} = Experiment,
    ExpId = cal_exp:exp_id(),

    lists:foreach(
        fun(#{<<"replicas">> := Replicas}=EntrySpec) ->
            {Start, End} = get_workflow_info(EntrySpec),

            lists:foreach(
                fun(PodId) ->
                    %% pod body
                    Body = cal_exp:pod_body(ExpId,
                                            PodId,
                                            EntrySpec),

                    %% schedule pod
                    cal_scheduler:schedule_pod(Body, Start, End)
                end,
                lists:seq(1, Replicas)
            )
        end,
        EntrySpecs
    ).

%% @private Get workflow info.
%%           - Default start: now
%%           - Default stop : never
-spec get_workflow_info(maps:map()) ->
    {now | event(), never | event()}.
get_workflow_info(EntrySpec) ->
    Workflow = maps:get(<<"workflow">>, EntrySpec, #{}),
    {parse_info(maps:get(<<"start">>, Workflow, now)),
     parse_info(maps:get(<<"stop">>,  Workflow, never))}.

%% @private
parse_info(I) when is_atom(I) ->
    I;
parse_info({Name, Value}) when is_binary(Name), is_binary(Value) ->
    {Name, binary_to_integer(Value)}.
