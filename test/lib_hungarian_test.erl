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

init_labels_test_() ->
    Helper = #hungarian_helper{
                cost = lib_matrix:new(3, 3, ?INT16, 
                                      [7, 4, 3, 3, 1, 2, 3, 0, 0]),
                unit = ?INT16,
                n = 3
               },
    #hungarian_helper{
       xy = BinXYInit
      } = InitHelper = lib_hungarian:init_labels(Helper),
    {Root, Queue, BinPrev, BinSource, BinTarget} = ParamHelper = 
        lib_hungarian:inner_init_param(3, BinXYInit),
    [?_assertEqual(InitHelper#hungarian_helper.lx, <<0, 7, 0, 3, 0, 3>>),
     ?_assertEqual(InitHelper#hungarian_helper.ly, <<0, 0, 0, 0, 0, 0>>),
     ?_assertEqual(Root, 1),
     ?_assertEqual(Queue, lib_wr_queue:in(1, lib_wr_queue:new(?INT8, 3))),
     ?_assertEqual(BinPrev, <<-2, -1, -1>>),
     ?_assertEqual(BinSource, <<1:1, 0:1, 0:1>>),
     ?_assertEqual(BinTarget, <<0:1, 0:1, 0:1>>)
    ].

hungarian_test_() ->
    Cost = lib_matrix:new(3, 3, ?INT16, [7, 4, 3, 3, 1, 2, 3, 0, 0]),
    List = [], %% lib_hungarian:hungarian(Cost),
    [?_assertEqual(List, [])].

