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
    [?_assertEqual(1, lib_matrix:get(1, 1, Init)),
     ?_assertEqual(5, lib_matrix:get(1, 5, Init)),
     ?_assertEqual(6, lib_matrix:get(2, 1, Init)),
     ?_assertEqual(46, lib_matrix:get(10, 1, Init))].

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

add_test_() ->
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
     ?_assertEqual(lib_matrix:add(Init, Init), InitPlus)
    ].

