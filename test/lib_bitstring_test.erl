%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_bitstring_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

-include("define_matrix.hrl").

index_test_() ->
    Bin = <<<<V:?INT16>> || V <- lists:seq(1, 100)>>,
    Bin2 = <<<<V:?INT32>> || V <- [5, 5, 5, 5, 5, 10, 10, 10, 10, 10]>>,
    Bin3 = <<<<V:?INT32>> || V <- [5, 5, 5, 5, 5, -1, 10, 10, 10, -10]>>,
    [?_assertEqual(lib_bitstring:index(1, ?INT16, Bin), 1),
     ?_assertEqual(lib_bitstring:index(100, ?INT16, Bin), 100),
     ?_assertEqual(lib_bitstring:index(50, ?INT16, Bin), 50),
     ?_assertEqual(lib_bitstring:index(101, ?INT16, Bin), false),
     ?_assertEqual(lib_bitstring:index(5, ?INT32, Bin2), 1),
     ?_assertEqual(lib_bitstring:index(10, ?INT32, Bin2), 6),
     ?_assertEqual(lib_bitstring:index(-1, ?INT32, Bin3), 6),
     ?_assertEqual(lib_bitstring:index(-10, ?INT32, Bin3), 10)
    ].

get_test_() ->
    Bin = <<<<V:?INT32>> || V <- lists:seq(1, 100)>>,
    [?_assertEqual(lib_bitstring:get(1, ?INT32, Bin), 1),
     ?_assertEqual(lib_bitstring:get(50, ?INT32, Bin), 50),
     ?_assertEqual(lib_bitstring:get(100, ?INT32, Bin), 100)].

update_test_() ->
    Bin = <<<<V:?INT16>> || V <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]>>,
    Bin2 = <<<<V:?INT16>> || V <- [1, 3, 3, 4, 5, 6, 7, 8, 9, 10]>>,
    Bin3 = <<<<V:?INT16>> || V <- [1, 2, 3, 4, 10, 6, 7, 8, 9, 10]>>,
    [?_assertEqual(lib_bitstring:update(2, ?INT16, 3, Bin), Bin2),
     ?_assertEqual(lib_bitstring:update(5, ?INT16, 10, Bin), Bin3)].

zipfoldl_test_() ->
    BinA = <<<<V:?INT1>> || V <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]>>,
    BinB = <<<<V:?INT32>> || V <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]>>,
    [?_assertEqual(lib_bitstring:zipfoldl(
                     fun (ValueA, ValueB, InAcc) ->
                             if
                                 ValueA =:= 1 ->
                                     InAcc + ValueB;
                                 true ->
                                     InAcc
                             end
                     end, 0, ?INT1, BinA, ?INT32, BinB),
                   25)].

