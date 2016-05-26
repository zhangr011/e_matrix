%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_wr_queue_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

-include("define_matrix.hrl").
-include("define_queue.hrl").

put_get_reset_index_test_() ->
    Queue = lib_wr_queue:new(?INT16, 200),
    Queue100 = lists:foldl(
                 fun (Value, InQueue) ->
                         lib_wr_queue:in(Value, InQueue)
                 end, Queue, lists:seq(1, 100)),
    {Value1, Queue1} = lib_wr_queue:get(Queue100),
    {Value2, Queue2} = lib_wr_queue:get(Queue1),
    QueueReset = lib_wr_queue:reset_index(Queue2),
    [?_assertEqual(Queue100#wr_queue.wr, 100),
     ?_assertEqual(Value1, 1),
     ?_assertEqual(Queue1#wr_queue.rd, 1),
     ?_assertEqual(Value2, 2),
     ?_assertEqual(Queue2#wr_queue.rd, 2),
     ?_assertEqual(QueueReset#wr_queue.rd, 0),
     ?_assertEqual(QueueReset#wr_queue.wr, 0)].

