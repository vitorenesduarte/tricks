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

-module(cal_example).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("cal.hrl").

%% API
-export([hello_world/0]).

-define(SEP, <<"-">>).

%% @doc Run hello world example.
-spec hello_world() -> ok.
hello_world() ->
   Exp = #{<<"apiVersion">> => <<"v1">>,
           <<"experiment">> =>
           [#{<<"tag">> => <<"hello-world">>,
              <<"image">> => <<"vitorenesduarte/cal-example">>,
              <<"replicas">> => 1,
              <<"env">> =>
              [#{<<"name">> => <<"TYPE">>,
                 <<"value">> => <<"hello-world">>},
               #{<<"name">> => <<"COUNT">>,
                 <<"value">> => <<"1000">>}]}]},

   cal:run(Exp).
