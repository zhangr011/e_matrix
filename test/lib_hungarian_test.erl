%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_hungarian_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

-include("define_hungarian.hrl").
-include("define_matrix.hrl").
-include("define_queue.hrl").

%% init_labels_test_() ->
%%     Helper = #hungarian_helper{
%%                 cost = lib_matrix:new(3, 3, ?INT16, 
%%                                       [6, 4, 3, 3, 1, 2, 3, 0, 0]),
%%                 unit = ?INT16,
%%                 n = 3
%%                },
%%     InitHelper = lib_hungarian:init_labels(Helper),
%%     [?_assertEqual(PreHelper#hungarian_helper.lx, <<0, 6, 0, 3, 0, 3>>),
%%      ?_assertEqual(PreHelper#hungarian_helper.ly, <<0, 0, 0, 0, 0, 0>>),
%%      ?_assertEqual(PreHelper#hungarian_helper.queue,
%%                    lib_wr_queue:in(1, lib_wr_queue:new(?INT8, ?N))),
%%      ?_assertEqual(PreHelper#hungarian_helper.prev, <<-2, -1, -1>>),
%%      ?_assertEqual(PreHelper#hungarian_helper.source, <<1:1, 0:1, 0:1>>),
%%      ?_assertEqual(PreHelper#hungarian_helper.target, <<0:1, 0:1, 0:1>>),
%%      ?_assertEqual(PreHelper#hungarian_helper.slack, <<0, 0, 0, 2, 0, 3>>),
%%      ?_assertEqual(PreHelper#hungarian_helper.slackx, <<1, 1, 1>>)
%%     ].

hungarian_test_() ->
    Cost = lib_matrix:new(3, 3, ?INT16, [6, 4, 3, 3, 1, 2, 3, 0, 0]),
    Cost2 = lib_matrix:new(10, 10, ?INT32, 
                           [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
                            10, 9, 8, 7, 6, 5, 4, 3, 2, 1,
                            1, 3, 5, 7, 9, 10, 8, 6, 4, 2,
                            2, 4, 6, 8, 10, 9, 7, 5, 3, 1,
                            1, 9, 5, 8, 9, 6, 6, 4, 6, 3,
                            5, 12, 7, 3, 18, 11, 0, 12, 17, 9,
                            32, 11, 5, 3, 4, 2, 8, 6, 17, 10,
                            21, 1, 37, 8, 7, 32, 2, 3, 1, 5,
                            12, 50, 17, 0, 32, 5, 33, 17, 8, 9,
                            9, 11, 3, 5, 6, 3, 2, 1, 1, 1]),
    [?_assertEqual(lib_hungarian:hungarian(Cost), [2, 3, 1]),
     ?_assertEqual(lib_hungarian:hungarian(Cost2), 
                   [10, 4, 6, 5, 7, 9, 1, 3, 2, 8])
    ].

