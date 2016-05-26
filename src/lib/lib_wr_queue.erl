%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created : 25 May 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_wr_queue).

%% API
-export([new/2, in/2, get/1, reset_index/1]).

-include("define_queue.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc new wr queue, queue with wr and rd index, put item then wr + 1, get item then rd + 1
-spec new(Unit :: pos_integer(), Size :: pos_integer()) ->
                 #wr_queue{}.
new(Unit, Size) ->
    #wr_queue{
       unit = Unit,
       data = << <<V:Unit>> || V <- lists:duplicate(Size, 0) >>,
       wr = 0,
       rd = 0
      }.

%% @doc put item in queue, then wr + 1
-spec in(Value :: integer(), #wr_queue{}) ->
                #wr_queue{}.
in(Value, #wr_queue{
             unit = Unit,
             data = Data,
             wr = Wr
            } = Queue) ->
    Next = Wr + 1,
    Queue#wr_queue{
      data = lib_bitstring:update(Next, Unit, Value, Data),
      wr = Next
     }.

%% @doc get item from queue, then rd + 1
-spec get(Queue :: #wr_queue{}) ->
                 {integer(), #wr_queue{}}.
get(#wr_queue{
       unit = Unit,
       data = Data,
       rd = Index
      } = Queue) ->
    Next = Index + 1,
    Value = lib_bitstring:get(Next, Unit, Data),
    {Value, Queue#wr_queue{rd = Next}}.

%% @doc reset the wr and rd index to default
-spec reset_index(Queue :: #wr_queue{}) ->
                         #wr_queue{}.
reset_index(Queue) ->
    Queue#wr_queue{
      wr = 0,
      rd = 0
     }.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
