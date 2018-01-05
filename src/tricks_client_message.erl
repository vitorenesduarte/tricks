%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Vitor Enes.  All Rights Reserved.
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

%% @doc Client socket message encoding.

-module(tricks_client_message).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

%% API
-export([decode/1,
         encode_notification/2]).

%% @doc Decode message.
-spec decode(binary()) -> maps:map().
decode(Bin) ->
    tricks_util:parse_json(Bin).

%% @doc Encode notification.
-spec encode_notification(exp_id(), event()) -> binary().
encode_notification(_ExpId, _Event) ->
    <<>>.
