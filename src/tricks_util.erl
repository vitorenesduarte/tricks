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

-module(tricks_util).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("tricks.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([binary_join/1,
         binary_join/2,
         parse_binary/1,
         parse_integer/1,
         parse_event/1,
         parse_pod_data/1,
         parse_json/1,
         compose_json/1,
         dict_find/3]).

%% @doc Join a list of binaries.
-spec binary_join(list(binary())) -> binary().
binary_join(List) ->
    binary_join(<<>>, List).

%% @doc Join a list of binaries using a given separator.
-spec binary_join(binary(), list(binary())) -> binary().
binary_join(_Sep, []) ->
    <<>>;
binary_join(Sep, List) ->
    binary_join(Sep, List, <<>>).

%% @doc Parse a binary.
-spec parse_binary(term()) -> binary().
parse_binary(A) when is_binary(A) ->
    A;
parse_binary(A) when is_integer(A) ->
    integer_to_binary(A);
parse_binary(A) when is_list(A) ->
    list_to_binary(A);
parse_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8).

%% @doc Parse an integer.
-spec parse_integer(term()) -> integer().
parse_integer(A) when is_integer(A) ->
    A;
parse_integer(A) when is_binary(A) ->
    binary_to_integer(A).

%% @doc Parse an event.
-spec parse_event(term()) -> event().
parse_event({A, B}) ->
    {parse_binary(A), parse_integer(B)}.

%% @doc Parse pod data.
-spec parse_pod_data(term()) -> pod_data().
parse_pod_data({A, B}) ->
    {parse_integer(A), parse_binary(B)}.

%% @doc Parse JSON. Return a map where labels are atoms.
-spec parse_json(binary()) -> maps:map().
parse_json(A) ->
    jsx:decode(A, [return_maps, {labels, atom}]).

%% @doc Compose JSON.
-spec compose_json(maps:map()) -> binary().
compose_json(A) ->
    jsx:encode(A).

%% @doc Find a key in a dictionary,
%%      returning a default in case it's not found.
-spec dict_find(term(), dict:dict(), term()) -> term().
dict_find(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, V} -> V;
        error -> Default
    end.

%% @private
binary_join(_Sep, [E], Bin) ->
    EBin = parse_binary(E),
    <<Bin/binary, EBin/binary>>;
binary_join(Sep, [E|Rest], Bin) ->
    EBin = parse_binary(E),
    binary_join(Sep, Rest, <<Bin/binary, EBin/binary, Sep/binary>>).


%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).

binary_join_test() ->
    L0 = [],
    L1 = [<<"a">>],
    L2 = [<<"a">>, <<"b">>],
    L3 = [<<"a">>, <<"b">>, <<"c">>],

    Sep0 = <<>>,
    Sep1 = <<";">>,
    Sep2 = <<"%3D">>,

    ?assertEqual(<<>>, binary_join(L0)),
    ?assertEqual(<<>>, binary_join(Sep0, L0)),
    ?assertEqual(<<>>, binary_join(Sep1, L0)),
    ?assertEqual(<<"a">>, binary_join(Sep0, L1)),
    ?assertEqual(<<"a">>, binary_join(Sep1, L1)),
    ?assertEqual(<<"ab">>, binary_join(Sep0, L2)),
    ?assertEqual(<<"a;b">>, binary_join(Sep1, L2)),
    ?assertEqual(<<"abc">>, binary_join(Sep0, L3)),
    ?assertEqual(<<"a;b;c">>, binary_join(Sep1, L3)),
    ?assertEqual(<<"a%3Db%3Dc">>, binary_join(Sep2, L3)).

-endif.
