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

-module(cal_util).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

-include("cal.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([binary_join/2,
         parse/2]).

%% @doc Join a list of binaries using a given separator.
-spec binary_join(binary(), list(binary())) -> binary().
binary_join(_Sep, []) ->
    <<>>;
binary_join(Sep, List) ->
    binary_join(Sep, List, <<>>).

%% @doc Parse a given term.
parse(binary, A) when is_binary(A) ->
    A;
parse(integer, A) when is_integer(A) ->
    A;
parse(event, A) ->
    parse({binary, integer}, A);
parse({T1, T2}, {A, B}) ->
    {parse(T1, A), parse(T2, B)};

parse(binary, A) when is_integer(A) ->
    integer_to_binary(A);
parse(binary, A) when is_list(A) ->
    list_to_binary(A);
parse(binary, A) when is_atom(A) ->
    atom_to_binary(A, utf8);

parse(integer, A) when is_binary(A) ->
    binary_to_integer(A);
parse(integer, A) when is_list(A) ->
    list_to_integer(A).


%% @private
binary_join(_Sep, [E], Bin) ->
    <<Bin/binary, E/binary>>;
binary_join(Sep, [E|Rest], Bin) ->
    binary_join(Sep, Rest, <<Bin/binary, E/binary, Sep/binary>>).


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
