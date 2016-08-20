%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created : 25 May 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_hungarian).

%% API
-export([hungarian/1]).

-ifdef(TEST).

-export([init_labels/1,
         inner_update_labels/1,
         inner_augment_pre/1,
         inner_build_bfs_tree/1,
         inner_check_runtimes/0
        ]).

-endif.

-include("define_hungarian.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 
-spec hungarian(#matrix{}) ->
                       list().
hungarian(#matrix{
             column = Columns,
             unit = Unit
            } = Cost) ->
    InitHelper = init_labels(#hungarian_helper{
                                unit = Unit,
                                cost = Cost,
                                n = Columns,
                                max_match = 0
                               }),
    #hungarian_helper{
       xy = BinXY
      } = augment(InitHelper),
    binary_to_list(BinXY).

init_labels(#hungarian_helper{
               unit = Unit,
               cost = CostMatrix,
               n = Columns
              } = Helper) ->
    Bin = << <<Value:?INT8>> || 
              Value <- lists:duplicate(Columns, ?MINUS_ONE)>>,
    Helper#hungarian_helper{
      lx = lib_matrix:row_max(CostMatrix),
      ly = << <<Value:Unit>> || Value <- lists:duplicate(Columns, 0)>>,
      xy = Bin,
      yx = Bin
     }.

augment(#hungarian_helper{
           n = Same,
           max_match = Same
          } = Helper) ->
    %% matching is already perfect
    %% ?debugMsg("augment"),
    Helper;
augment(Helper) ->
    %% ?debugMsg("augment"),
    PreHelper = inner_augment_pre(Helper),
    inner_augment(PreHelper).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec inner_augment_pre(Helper :: #hungarian_helper{}) ->
                               PreHelper :: #hungarian_helper{}.
inner_augment_pre(#hungarian_helper{
                     cost = #matrix{
                               column = Column
                              } = Cost,
                     lx = BinLx,
                     ly = BinLy,
                     xy = BinXY
                    } = Helper) ->
    {Root, Queue, BinPrev, BinSource, BinTarget} = 
        inner_init_param(Column, BinXY),
    %% initializing slack array
    BinSlack = inner_init_slack(Root, BinLx, BinLy, Cost),
    BinSlackX = << <<Value:?INT8>> || 
                    Value <- lists:duplicate(Column, Root)>>,
    %% ?debugFmt("S: ~p", [BinSource]),
    %% ?debugFmt("slack: ~p", [BinSlack]),
    %% ?debugFmt("slackx: ~p", [BinSlackX]),
    %% ?debugFmt("lx: ~p", [BinLx]),
    Helper#hungarian_helper{
      source = BinSource,  %% init set source
      target = BinTarget,  %% init set target
      slack = BinSlack,
      slackx = BinSlackX,
      %% init set prev - for the alternating tree
      prev = BinPrev,
      queue = Queue
     }.

-spec inner_init_param(Column :: pos_integer(),
                       BinXY :: bitstring()) ->
                              {Root :: pos_integer(),
                               Queue :: #wr_queue{},
                               BinPrev :: bitstring(),
                               BinSource :: bitstring(),
                               BinTarget :: bitstring()}.
inner_init_param(Column, BinXY) ->
    BinBoolInit = << <<V:?INT1>> || V <- lists:duplicate(Column, 0) >>,
    BinPrevInit = 
        << <<V:?INT8>> || V <- lists:duplicate(Column, ?MINUS_ONE) >>,
    Queue = lib_wr_queue:new(?INT8, Column),
    case lib_bitstring:index(?MINUS_ONE, ?INT8, BinXY) of
        false ->
            {1, Queue, BinPrevInit, BinBoolInit, BinBoolInit};
        IndexX ->
            %% finding root of the tree
            UpdateQueue = lib_wr_queue:in(IndexX, Queue),
            BinPrevUpdate = 
                lib_bitstring:update(IndexX, ?INT8, -2, BinPrevInit),
            BinSource = lib_bitstring:update(IndexX, ?INT1, 1, BinBoolInit),
            {IndexX, UpdateQueue, BinPrevUpdate, BinSource, BinBoolInit}
    end.

-spec inner_init_slack(Root :: pos_integer(),
                       BinLx :: bitstring(),
                       BinLy :: bitstring(),
                       Cost :: #matrix{}) ->
                              bitstring().
inner_init_slack(Root, BinLx, BinLy, #matrix{
                                        unit = Unit
                                       } = Cost) ->
    LxRoot = lib_bitstring:get(Root, Unit, BinLx),
    BinCostRoot = lib_matrix:row(Root, Cost),
    lib_bitstring:zipwith(fun (ValueLy, CostRoot) ->
                                  LxRoot + ValueLy - CostRoot
                          end, Unit, BinLy, BinCostRoot).

-spec inner_update_labels(#hungarian_helper{}) ->
                                 #hungarian_helper{}.
inner_update_labels(#hungarian_helper{
                       cost = #matrix{
                                 unit = Unit
                                },
                       source = BinSource,
                       target = BinTarget,
                       lx = BinLx,
                       ly = BinLy,
                       slack = BinSlack
                      } = Helper) ->
    %% calculate delta using slack
    Delta = lib_bitstring:zipfoldl(
              fun (0, ValueSlack, InDelta) ->
                      min(InDelta, ValueSlack);
                  (_ValueTarget, _ValueSlack, InDelta) ->
                      InDelta
              end, undefined, ?INT1, BinTarget, Unit, BinSlack),
    %% update X labels
    NewBinLx = lib_bitstring:zipwith(
                 fun (1, ValueLx) ->
                         ValueLx - Delta;
                     (_ValueSource, ValueLx) -> 
                         ValueLx
                 end, ?INT1, BinSource, Unit, BinLx),
    %% update Y labels
    NewBinLy = lib_bitstring:zipwith(
                 fun (1, ValueLy) ->
                         ValueLy + Delta;
                     (_ValueTarget, ValueLy) ->
                         ValueLy
                 end, ?INT1, BinTarget, Unit, BinLy),
    %% update slack 
    NewBinSlack = lib_bitstring:zipwith(
                    fun (0, ValueSlack) ->  
                            ValueSlack - Delta;
                        (_ValueTarget, ValueSlack) ->
                            ValueSlack
                    end, ?INT1, BinTarget, Unit, BinSlack),
    %% ?debugFmt("update labels lx pre: ~p", [BinLx]),
    %% ?debugFmt("delta: ~p", [Delta]),
    %% ?debugFmt("S: ~p", [BinSource]),
    %% ?debugFmt("T: ~p", [BinTarget]),
    %% ?debugFmt("update labels lx: ~p", [NewBinLx]),
    %% ?debugFmt("update labels ly: ~p", [NewBinLy]),
    Helper#hungarian_helper{
      lx = NewBinLx,
      ly = NewBinLy,
      slack = NewBinSlack
     }.

-spec inner_add_to_tree(X :: pos_integer(),
                        PrevX :: pos_integer(),
                        Helper :: #hungarian_helper{}) ->
                               #hungarian_helper{}.
inner_add_to_tree(X, PrevX, #hungarian_helper{
                               source = BinSource,
                               cost = #matrix{
                                         column = Columns,
                                         unit = Unit
                                        } = Cost,
                               lx = BinLx,
                               ly = BinLy,
                               slack = BinSlack,
                               slackx = BinSlackX,
                               prev = BinPrev
                              } = Helper) ->
    %% x - current vertex,
    %% prevx - vertex from X before x in the alternating path,
    %% so we add edges (prevx, xy[x]), (xy[x], x)
    %% add X to source
    NewBinSource = lib_bitstring:update(X, ?INT1, 1, BinSource),
    %% we need this when augmenting
    NewBinPrev = lib_bitstring:update(X, ?INT8, PrevX, BinPrev),
    %% update slacks, because we add new vertex to source
    BinCostX = lib_matrix:row(X, Cost),
    ValueLx = lib_bitstring:get(X, Unit, BinLx),
    {NewBinSlack, NewBinSlackx} = 
        lists:foldl(
          fun (Index, {InBinSlack, InBinSlackX} = InAcc) -> 
                  ValueLy = lib_bitstring:get(Index, Unit, BinLy),
                  ValueCostXY = lib_bitstring:get(Index, Unit, BinCostX),
                  ValueSlack = lib_bitstring:get(Index, Unit, InBinSlack),
                  Value = ValueLx + ValueLy - ValueCostXY,
                  if
                      Value < ValueSlack ->
                          {lib_bitstring:update(
                             Index, Unit, Value, InBinSlack),
                           lib_bitstring:update(
                             Index, ?INT8, X, InBinSlackX)};
                      true ->
                          InAcc
                  end
          end, {BinSlack, BinSlackX}, lists:seq(1, Columns)),
    %% ?debugFmt("add to tree S: ~p", [NewBinSource]),
    %% ?debugFmt("add to tree prev: ~p", [NewBinPrev]),
    %% ?debugFmt("add to tree slack: ~p", [NewBinSlack]),
    %% ?debugFmt("add to tree slackx: ~p", [NewBinSlackx]),
    Helper#hungarian_helper{
      source = NewBinSource,
      slack = NewBinSlack,
      slackx = NewBinSlackx,
      prev = NewBinPrev
     }.

-spec inner_augment(#hungarian_helper{}) ->
                           #hungarian_helper{}.
inner_augment(Helper) ->
    %% inner_check_runtimes(),
    case inner_build_bfs_tree(Helper) of
        {break, X, Y, NewHelper} ->
            inner_augment_found(X, Y, NewHelper);
        {ok, NewHelper} ->
            inner_augment_advance(NewHelper)
    end.

-spec inner_build_bfs_tree(#hungarian_helper{}) ->
                                  term().
inner_build_bfs_tree(#hungarian_helper{
                        queue = #wr_queue{
                                   wr = Same,
                                   rd = Same
                                  }
                       } = Helper) ->
    %% queue get over all item
    {ok, Helper};
inner_build_bfs_tree(#hungarian_helper{
                        cost = #matrix{
                                  column = Columns,
                                  unit = Unit
                                 } = Cost,
                        queue = Queue
                       } = Helper) ->
    {X, QueueGet} = lib_wr_queue:get(Queue),
    BinCostX = lib_matrix:row(X, Cost),
    GetHelper = Helper#hungarian_helper{
                  queue = QueueGet
                 },
    %% iterate through all edges in equality graph
    case inner_iter_all_edges(1, Columns, Unit, BinCostX, X, GetHelper) of
        {break, IndexY} ->
            {break, X, IndexY, GetHelper};
        {ok, EdgesHelper} ->
            inner_build_bfs_tree(EdgesHelper)
    end.

%% @doc augmenting path not found, so improve labeling
-spec inner_augment_advance(#hungarian_helper{}) ->
                                   term().
inner_augment_advance(#hungarian_helper{
                         n = Columns
                        } = Helper) ->
    #hungarian_helper{
       queue = LabelQueue
      } = LabelHelper = inner_update_labels(Helper),
    ResetHelper = LabelHelper#hungarian_helper{
                    queue = lib_wr_queue:reset_index(LabelQueue)
                   },
    case inner_iter_equality_graph(1, Columns, ResetHelper) of
        {break, X, IndexY} ->
            inner_augment_found(X, IndexY, ResetHelper);
        {ok, FinalHelper} ->
            inner_augment(FinalHelper)
    end.

-spec inner_iter_all_edges(Index :: pos_integer(),
                           Max :: pos_integer(),
                           Unit :: pos_integer(),
                           BinCostX :: bitstring(),
                           X :: pos_integer(),
                           Helper :: #hungarian_helper{}) ->
                                  {break, pos_integer()} |
                                  {ok, #hungarian_helper{}}.
inner_iter_all_edges(Index, Max, _, _, _, Helper) when Index > Max ->
    {ok, Helper};
inner_iter_all_edges(Index, Max, Unit, BinCostX, X, #hungarian_helper{
                                                       target = BinTarget,
                                                       lx = BinLx,
                                                       ly = BinLy,
                                                       yx = BinYX,
                                                       queue = Queue
                                                      } = Helper) ->
    ValueCostXY = lib_bitstring:get(Index, Unit, BinCostX),
    ValueLx = lib_bitstring:get(X, Unit, BinLx),
    ValueLy = lib_bitstring:get(Index, Unit, BinLy),
    ValueTarget = lib_bitstring:get(Index, ?INT1, BinTarget),
    %% ?debugFmt("x: ~p, y: ~p, cost: ~p, lx: ~p, ly: ~p, ty: ~p", 
    %%           [X, Index, ValueCostXY, ValueLx, ValueLy, ValueTarget]),
    if
        ValueCostXY =:= ValueLx + ValueLy,
        ValueTarget =:= 0 ->
            BinIndex = lib_bitstring:get_bits(Index, ?INT8, BinYX),
            if
                BinIndex =:= ?BIN_MINUS_ONE_INT8 ->
                    %% an exposed vertex in Y found, 
                    %% so augmenting path exists!
                    {break, Index};
                true ->
                    <<ValueYx:?INT8>> = BinIndex,
                    %% add y to target
                    NewBinTarget = 
                        lib_bitstring:update(Index, ?INT1, 1, BinTarget),
                    %% add vertex yx[y],which is matched with y, to the queue
                    NewQueue = lib_wr_queue:in(ValueYx, Queue),
                    %% add edges (x, y) and (y, yx[y]) to the tree
                    AddTreeHelper = 
                        inner_add_to_tree(ValueYx, X, 
                                          Helper#hungarian_helper{
                                            target = NewBinTarget,
                                            queue = NewQueue
                                           }),
                    inner_iter_all_edges(
                      Index + 1, Max, Unit, BinCostX, X, AddTreeHelper)
            end;
        true ->
            inner_iter_all_edges(Index + 1, Max, Unit, BinCostX, X, Helper)
    end.

%% @doc in this cycle we add edges that were added to the equality graph as a
%% result of improving the labeling, we add edge (slackx[y], y) to the tree if
%% and only if !T[y] && slack[y] == 0, also with this edge we add another one
%% (y, yx[y]) or augment the matching, if y was exposed
-spec inner_iter_equality_graph(Index :: pos_integer(),
                                Max :: pos_integer(),
                                Helper :: #hungarian_helper{}) ->
                                       {break, pos_integer(), pos_integer()}|
                                       {ok, #hungarian_helper{}}.
inner_iter_equality_graph(Index, Max, Helper) when Index > Max ->
    {ok, Helper};
inner_iter_equality_graph(Index, Max, #hungarian_helper{
                                         cost = #matrix{
                                                   unit = Unit
                                                  },
                                         source = BinSource,
                                         target = BinTarget,
                                         yx = BinYX,
                                         slack = BinSlack,
                                         slackx = BinSlackX,
                                         queue = Queue
                                        } = Helper) ->
    %% ?debugFmt("inner_iter_equality_graph: ~p, ~p", [Index, Max]),
    ValueTarget = lib_bitstring:get(Index, ?INT1, BinTarget),
    ValueSlack = lib_bitstring:get(Index, Unit, BinSlack),
    if
        ValueTarget =:= 0, ValueSlack =:= 0 ->
            BinIndex = lib_bitstring:get_bits(Index, ?INT8, BinYX),
            if
                BinIndex =:= ?BIN_MINUS_ONE_INT8 ->
                    %% exposed vertex in Y found - augmenting path exists!
                    X = lib_bitstring:get(Index, ?INT8, BinSlackX),
                    {break, X, Index};
                true ->
                    <<ValueYx:?INT8>> = BinIndex,
                    NewBinTarget = 
                        lib_bitstring:update(Index, ?INT1, 1, BinTarget),
                    case lib_bitstring:get(ValueYx, ?INT1, BinSource) of
                        0 ->
                            %% add vertex yx[y], which is matched with y, 
                            %% to the queue
                            NewQueue = lib_wr_queue:in(ValueYx, Queue),
                            %% and add edges (x,y) and (y, yx[y]) to the tree
                            AddTreeHelper = 
                                inner_add_to_tree(
                                  ValueYx, 
                                  lib_bitstring:get(Index, ?INT8, BinSlackX),
                                  Helper#hungarian_helper{
                                    target = NewBinTarget,
                                    queue = NewQueue
                                   }),
                            inner_iter_equality_graph(
                              Index + 1, Max, AddTreeHelper);
                        _ ->
                            inner_iter_equality_graph(
                              Index + 1, Max, Helper#hungarian_helper{
                                                target = NewBinTarget
                                               })
                    end
            end;
        true ->
            inner_iter_equality_graph(Index + 1, Max, Helper)
    end.

%% @doc we found augmenting path!
-spec inner_augment_found(X :: pos_integer(), Y :: pos_integer(),
                          #hungarian_helper{}) ->
                                 term().
inner_augment_found(X, Y, #hungarian_helper{
                             max_match = MaxMatches,
                             xy = BinXY,
                             yx = BinYX,
                             prev = BinPrev
                            } = Helper) ->
    {NewBinXY, NewBinYX} =
        inner_inverse_edges(X, Y, BinXY, BinYX, BinPrev),
    %% recall function, go to step 1 of the algorithm
    augment(Helper#hungarian_helper{
              %% increment matching
              max_match = MaxMatches + 1,
              xy = NewBinXY,
              yx = NewBinYX
             }).

%% @doc in this cycle we inverse edges along augmenting path
-spec inner_inverse_edges(Cx :: pos_integer(), Cy :: pos_integer(), 
                          BinXY :: bitstring(), BinYX :: bitstring(), 
                          BinPrev :: bitstring()) ->
                                 {bitstring(), bitstring()}.
inner_inverse_edges(Cx, Cy, BinXY, BinYX, BinPrev) ->
    NewBinYX = lib_bitstring:update(Cy, ?INT8, Cx, BinYX),
    NewBinXY = lib_bitstring:update(Cx, ?INT8, Cy, BinXY),
    BinIndex = lib_bitstring:get_bits(Cx, ?INT8, BinPrev),
    if
        BinIndex =:= ?BIN_MINUS_TWO_INT8 ->
            %% ?debugFmt("cycle xy: ~p", [NewBinXY]),
            %% ?debugFmt("cycle yx: ~p", [NewBinYX]),
            {NewBinXY, NewBinYX};
        true ->
            <<Cx2:?INT8>> = BinIndex,
            Ty = lib_bitstring:get(Cx, ?INT8, BinXY),
            inner_inverse_edges(Cx2, Ty, NewBinXY, NewBinYX, BinPrev)
    end.

inner_check_runtimes() ->
    Times = case get(augment_rum_times) of
                undefined ->
                    1;
                Num ->
                    Num + 1
            end,
    if
        Times > 5 ->
            throw(times_out);
        true ->
            put(augment_rum_times, Times)
    end.

