%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_matrix_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

-include("define_matrix.hrl").

new_test_() ->
    Ok = lib_matrix:new(10, 5, ?INT8, lists:seq(1, 10 * 5)),
    %% Bad = lib_matrix:new(10, 5, ?INT8, []),
    [?_assert(is_record(Ok, matrix))].

get_test_() ->
    Init = lib_matrix:new(10, 5, ?INT8, lists:seq(1, 10 * 5)),
    Init2 = lib_matrix:new(10, 5, ?INT16, lists:seq(1, 10 * 5)),
    Init3 = lib_matrix:new(10, 5, ?INT1, lists:seq(1, 10 * 5)),
    [?_assertEqual(1, lib_matrix:get(1, 1, Init)),
     ?_assertEqual(5, lib_matrix:get(1, 5, Init)),
     ?_assertEqual(6, lib_matrix:get(2, 1, Init)),
     ?_assertEqual(46, lib_matrix:get(10, 1, Init)),
     ?_assertEqual(50, lib_matrix:get(10, 5, Init)),
     ?_assertEqual(46, lib_matrix:get(10, 1, Init2)),
     ?_assertEqual(50, lib_matrix:get(10, 5, Init2)),
     ?_assertEqual(1, lib_matrix:get(1, 1, Init3)),
     ?_assertEqual(1, lib_matrix:get(1, 5, Init3)),
     ?_assertEqual(0, lib_matrix:get(2, 1, Init3)),
     ?_assertEqual(0, lib_matrix:get(10, 1, Init3)),
     ?_assertEqual(0, lib_matrix:get(10, 5, Init3))
    ].

is_square_test_() ->
    Square = lib_matrix:new(10, 10, ?INT8, lists:seq(1, 10 * 10)),
    NoSquare = lib_matrix:new(10, 8, ?INT8, lists:seq(1, 10 * 8)),
    [?_assert(lib_matrix:is_square(Square)),
     ?_assertNot(lib_matrix:is_square(NoSquare))].

column_and_row_test_() ->
    Init = lib_matrix:new(10, 5, ?INT32, lists:seq(1, 10 * 5)),
    Init1 = lib_matrix:new(10, 5, ?INT1, lists:seq(1, 10 * 5)),
    [?_assertEqual(lib_matrix:column(1, Init),
                   <<<<I:?INT32>> ||
                       I <- [1, 6, 11, 16, 21, 26, 31, 36, 41, 46]>>),
     ?_assertEqual(lib_matrix:column(5, Init),
                   <<<<I:?INT32>> ||
                       I <- [5, 10, 15, 20, 25, 30, 35, 40, 45, 50]>>),
     ?_assertEqual(lib_matrix:row(1, Init),
                   <<<<I:?INT32>> || I <- [1, 2, 3, 4, 5]>>),
     ?_assertEqual(lib_matrix:row(2, Init),
                   <<<<I:?INT32>> || I <- [6, 7, 8, 9, 10]>>),
     ?_assertEqual(lib_matrix:row(10, Init),
                   <<<<I:?INT32>> || I <- [46, 47, 48, 49, 50]>>),
     ?_assertEqual(lib_matrix:column(1, Init1),
                   <<1:1, 0:1, 1:1, 0:1, 1:1, 0:1, 1:1, 0:1, 1:1, 0:1>>),
     ?_assertEqual(lib_matrix:column(5, Init1),
                   <<1:1, 0:1, 1:1, 0:1, 1:1, 0:1, 1:1, 0:1, 1:1, 0:1>>),
     ?_assertEqual(lib_matrix:row(1, Init1),
                   <<1:1, 0:1, 1:1, 0:1, 1:1>>),
     ?_assertEqual(lib_matrix:row(2, Init1),
                   <<0:1, 1:1, 0:1, 1:1, 0:1>>),
     ?_assertEqual(lib_matrix:row(10, Init1),
                   <<0:1, 1:1, 0:1, 1:1, 0:1>>)
    ].

all_test_() ->
    Init = lib_matrix:new(10, 10, ?INT16, lists:duplicate(10 * 10, 0)),
    Init2 = lib_matrix:new(10, 10, ?INT32, lists:seq(1, 10 * 10)),
    [?_assert(lib_matrix:all(fun (0) ->
                                     true;
                                 (_) ->
                                     false
                             end, Init)),
     ?_assertNot(lib_matrix:all(fun (0) ->
                                        true;
                                    (_) ->
                                        false
                                end, Init2))].

add_mult_test_() ->
    Init = lib_matrix:new(10, 10, ?INT8, lists:seq(1, 10 * 10)),
    Init2 = lib_matrix:new(10, 10, ?INT64,
                           lists:seq(1000000, 1000000 + 10 * 10 - 1)),
    InitPlus = lib_matrix:new(10, 10, ?INT8,
                              lists:map(fun (Id) ->
                                                Id * 2
                                        end,
                                        lists:seq(1, 10 * 10))),
    [?_assertEqual(Init, #matrix{
                            column = 10,
                            row = 10,
                            unit = ?INT8,
                            data = <<<<I:?INT8>> || I <- lists:seq(1, 100)>>
                           }),
     ?_assertEqual(lib_matrix:add(1, Init),
                   Init#matrix{
                     data = <<<<I:?INT8>> || I <- lists:seq(2, 101)>>
                    }),
     ?_assertEqual(lib_matrix:add(200, Init2),
                   Init2#matrix{
                     data = <<<<I:?INT64>> ||
                                I <- lists:seq(1000200,
                                               1000200 + 10 * 10 - 1)>>
                    }),
     ?_assertEqual(lib_matrix:add(Init, Init), InitPlus),
     ?_assertEqual(lib_matrix:mult(2, Init), InitPlus)
    ].

insert_row_test_() ->
    List1 = lists:seq(1, 10 * 10),
    Init = lib_matrix:new(10, 10, ?INT16, List1),
    List2 = lists:seq(201, 210),
    [?_assertEqual(lib_matrix:insert_row(0, List2, Init),
                   lib_matrix:new(11, 10, ?INT16, List2 ++ List1)),
     ?_assertEqual(lib_matrix:insert_row(undefined, List2, Init),
                   lib_matrix:new(11, 10, ?INT16, List1 ++ List2)),
     ?_assertEqual(lib_matrix:insert_row(100, List2, Init),
                   lib_matrix:new(11, 10, ?INT16, List1 ++ List2)),
     ?_assertEqual(lib_matrix:insert_row(5, List2, Init),
                   lib_matrix:new(11, 10, ?INT16,
                                  lists:seq(1, 50) ++ List2  ++
                                      lists:seq(51, 100)))
    ].

insert_column_test_() ->
    List1 = inner_column_list(8),
    Lists2 = [9, 19, 29, 39, 49, 59, 69, 79, 89, 99],
    Lists3 = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Lists4 = inner_column_list(9),
    Lists5 = inner_column_list(10),
    Init = lib_matrix:new(10, 8, ?INT16, List1),
    Init2 = lib_matrix:new(10, 9, ?INT16, Lists4),
    %% ?debugFmt("~p~n", [lib_matrix:new(10, 9, ?INT16, Lists4)]),
    [?_assertEqual(lib_matrix:insert_column(9, Lists2, Init),
                   Init2),
     ?_assertEqual(lib_matrix:insert_column(9, Lists3, Init2),
                   lib_matrix:new(10, 10, ?INT16, Lists5)),
     ?_assertEqual(lib_matrix:insert_column(0, Lists2, Init),
                   lib_matrix:new(10, 9, ?INT16,
                                  [9, 1, 2, 3, 4, 5, 6, 7, 8,
                                   19, 11, 12, 13, 14, 15, 16, 17, 18,
                                   29, 21, 22, 23, 24, 25, 26, 27, 28,
                                   39, 31, 32, 33, 34, 35, 36, 37, 38,
                                   49, 41, 42, 43, 44, 45, 46, 47, 48,
                                   59, 51, 52, 53, 54, 55, 56, 57, 58,
                                   69, 61, 62, 63, 64, 65, 66, 67, 68,
                                   79, 71, 72, 73, 74, 75, 76, 77, 78,
                                   89, 81, 82, 83, 84, 85, 86, 87, 88,
                                   99, 91, 92, 93, 94, 95, 96, 97, 98
                                  ])),
     ?_assertEqual(lib_matrix:insert_column(3, Lists2, Init),
                   lib_matrix:new(10, 9, ?INT16,
                                  [1, 2, 3, 9, 4, 5, 6, 7, 8,
                                   11, 12, 13, 19, 14, 15, 16, 17, 18,
                                   21, 22, 23, 29, 24, 25, 26, 27, 28,
                                   31, 32, 33, 39, 34, 35, 36, 37, 38,
                                   41, 42, 43, 49, 44, 45, 46, 47, 48,
                                   51, 52, 53, 59, 54, 55, 56, 57, 58,
                                   61, 62, 63, 69, 64, 65, 66, 67, 68,
                                   71, 72, 73, 79, 74, 75, 76, 77, 78,
                                   81, 82, 83, 89, 84, 85, 86, 87, 88,
                                   91, 92, 93, 99, 94, 95, 96, 97, 98
                                  ]))
    ].

hungarian_reduction_test_() ->
    List = [1, 2, 3, 4,
            3, 3, 4, 4,
            6, 2, 4, 1,
            7, 9, 9, 7],
    %% 1 2 3 4    0 1 2 3    0 1 1 3
    %% 3 3 4 4    0 0 1 1    0 0 0 1
    %% 6 2 4 1 -> 5 1 3 0 -> 5 1 2 0
    %% 7 9 9 7    0 2 2 0    0 2 1 0
    List2 = [0, 1, 1, 3,
             0, 0, 0, 1,
             5, 1, 2, 0,
             0, 2, 1, 0],
    Init = lib_matrix:new(4, 4, ?INT16, List),
    [?_assertEqual(lib_matrix:hungarian_reduction(Init),
                   lib_matrix:new(4, 4, ?INT16, List2))].

column_max_test_() ->
    Init = lib_matrix:new(10, 5, ?INT16, lists:seq(1, 10 * 5)),
    List = lists:seq(46, 50),
    [?_assertEqual(<< <<V:?INT16>> || V <- List >>,
                   lib_matrix:column_max(Init))].

transpose_test_() ->
  First = lib_matrix:new(10, 5, ?INT16, lists:seq(1, 10 * 5)),
  TFirst = lib_matrix:transpose(First),
  Second = lib_matrix:new(10, 10, ?INT16, lists:seq(1, 10 * 10)),
  TSecond = lib_matrix:transpose(Second),
  [?_assertEqual(First,lib_matrix:transpose(TFirst)),
  ?_assertEqual(Second,lib_matrix:transpose(TSecond))].

%% ============================== for inner ==============================

inner_column_list(Delta) ->
    lists:seq(1, Delta) ++ lists:seq(11, Delta + 10) ++
        lists:seq(21, Delta + 20) ++ lists:seq(31, Delta + 30) ++
        lists:seq(41, Delta + 40) ++ lists:seq(51, Delta + 50) ++
        lists:seq(61, Delta + 60) ++ lists:seq(71, Delta + 70) ++
        lists:seq(81, Delta + 80) ++ lists:seq(91, Delta + 90).

