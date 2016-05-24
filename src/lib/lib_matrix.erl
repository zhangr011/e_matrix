%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  1 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_matrix).

%% API
-export([new/4,
         is_square/1,

         get/3,
         column/2,
         row/2,

         add/2,
         mult/2,
         all/2,
         zipwith/3,

         insert_row/3,
         insert_column/3
        ]).

-include("define_matrix.hrl").



%%%===================================================================
%%% API
%%%===================================================================

%% @doc 
-spec new(pos_integer(), pos_integer(), pos_integer(), list()) ->
                 #matrix{}.
new(Row, Column, Unit, List) when Column * Row =:= length(List) ->
    {Min, Max} = inner_min_max_of_lists(List),
    #matrix{
       column = Column,
       row = Row,
       unit = Unit,
       data = << <<D:Unit>> || D <- List >>,
       min = Min,
       max = Max
      };
new(_, _, _, _) ->
    throw(error_bad_length).

%% @doc True if the matrix is square, false otherwise.
-spec is_square(#matrix{}) -> boolean().
is_square(#matrix{row = Rows, column = Columns})
  when Rows =:= Columns ->
    true;
is_square(_) ->
    false.

%% @doc get element of matrix
-spec get(pos_integer(), pos_integer(), #matrix{}) -> integer().
get(RowIndex, _Column, #matrix{
                          row = Rows
                         }) when RowIndex > Rows orelse RowIndex < 1 ->
    throw(out_of_row);
get(_Row, ColumnIndex, #matrix{
                          column = Columns
                         }) when ColumnIndex > Columns orelse Columns < 1 ->
    throw(out_of_column);
get(RowIndex, ColumnIndex, #matrix{
                              column = Columns,
                              data = Data
                             }) ->
    binary:at(Data, ((RowIndex - 1) * Columns + ColumnIndex - 1)).

%% @doc The vector representing the column at the given index.
-spec column(pos_integer(), #matrix{}) -> [integer()].
column(ColumnIndex, #matrix{
                       column = Columns
                      }) when ColumnIndex > Columns orelse 
                              ColumnIndex < 1 ->
    throw(out_of_column);
column(ColumnIndex, #matrix{
                       column = Columns,
                       unit = Unit,
                       data = Data
                      }) ->
    inner_filter_binary(Unit, 
                        fun (Index) 
                              when (Index - 1) rem Columns + 1 =:= 
                                   ColumnIndex ->
                                true;
                            (_) ->
                                false
                        end, Data).

%% @doc The vector representing the row at the given index.
-spec row(pos_integer(), #matrix{}) -> [integer()].
row(RowIndex, #matrix{
                 row = Rows
                })
  when RowIndex > Rows orelse RowIndex < 1 ->
    throw(out_of_row);
row(RowIndex, #matrix{
                 column = Columns,
                 unit = Unit,
                 data = Data
                }) ->
    if
        Unit >= ?INT8 ->
            binary:part(Data, 
                        (RowIndex - 1) * Columns * Unit div ?INT8, 
                        Columns * Unit div ?INT8);
        true ->
            PreIndex = (RowIndex - 1) * Columns * Unit,
            RowLength = Columns * Unit,
            <<_Head:PreIndex, Result:RowLength, _/bits>> = Data,
            <<Result:RowLength>>
    end.

%% @doc add for all element in matrix
-spec add(integer(), #matrix{}) -> 
                 #matrix{};
         (#matrix{}, #matrix{}) ->
                 #matrix{}.
add(Scalar, #matrix{
               unit = Unit,
               data = Original,
               min = Min,
               max = Max
              } = Matrix) when is_integer(Scalar) ->
    Matrix#matrix{
      data = << <<(X + Scalar):Unit>> || <<X:Unit>> <= Original>>,
      min = Min + Scalar,
      max = Max + Scalar
     };
add(#matrix{} = MatrixA, #matrix{} = MatrixB) ->
    %% matrix with same Column, Row and Unit can be added.
    zipwith(fun (A, B) ->
                    A + B
            end, MatrixA, MatrixB);
add(_, _) ->
    throw(not_match_matrix).

%% @doc mult for matrix
-spec mult(integer(), #matrix{}) ->
                  #matrix{};
          (#matrix{}, #matrix{}) ->
                  #matrix{}.
mult(Scalar, #matrix{
                unit = Unit,
                data = Original,
                min = Min,
                max = Max
               } = Matrix) when is_integer(Scalar) ->
    Matrix#matrix{
      data = <<<<(X * Scalar):Unit>> || <<X:Unit>> <= Original>>,
      min = Min * Scalar,
      max = Max * Scalar
     };
mult(#matrix{
        %% column = Columns,
        %% row = Fixed,
        %% unit = Unit,
        %% data = BinA
       }, #matrix{
             %% column = Fixed,
             %% row = Rows,
             %% unit = Unit,
             %% data = BinB
            }) ->
    <<>>;
mult(_, _) ->
    throw(not_match_matrix).

%% @doc 
-spec all(fun ((integer()) -> boolean()),
          #matrix{}) ->
                 boolean().
all(Pred, #matrix{
             unit = Unit,
             data = Bin
            }) ->
    inner_all(Pred, Unit, Bin).

%% @doc iterate matrix
-spec zipwith(fun((integer(), integer()) -> integer()), 
              #matrix{}, #matrix{}) ->
                     #matrix{}.
zipwith(Fun, #matrix{
                column = Columns,
                row = Rows,
                unit = Unit,
                data = BinDataA
               }, #matrix{
                     column = Columns,
                     row = Rows,
                     unit = Unit,
                     data = BinDataB
                    } = Matrix) ->
    NewBinData = inner_zipwith(Fun, Unit, BinDataA, BinDataB),
    {Min, Max} = inner_min_max_of_binary(Unit, NewBinData),
    Matrix#matrix{
      data = NewBinData,
      min = Min,
      max = Max
     };
zipwith(_, _, _) ->
    throw(not_match_matrix).

%% @doc insert row into matrix
-spec insert_row(pos_integer(), list(), #matrix{}) ->
                        #matrix{}.
insert_row(RowIndex, List, #matrix{
                              column = Columns,
                              row = Rows,
                              unit = Unit,
                              data = Bin,
                              min = Min,
                              max = Max
                             } = Matrix) when is_list(List),
                                              length(List) =:= Columns ->
    InsertBin = <<<<I:Unit>> || I <- List>>,
    NewBin = if
                 RowIndex =:= 0 ->
                     <<InsertBin/bits, Bin/bits>>;
                 RowIndex =:= undefined;
                 RowIndex >= Rows ->
                     <<Bin/bits, InsertBin/bits>>;
                 true ->
                     HeadLength = RowIndex * Columns * Unit,
                     <<Head:HeadLength, Rest/bits>> = Bin,
                     <<Head:HeadLength, InsertBin/bits, Rest/bits>>
             end,
    {RowMin, RowMax} = inner_min_max_of_lists(List),
    Matrix#matrix{
      row = Rows + 1,
      data = NewBin,
      min = min(Min, RowMin),
      max = max(Max, RowMax)
     };
insert_row(_, _, _) ->
    throw(not_match_columns).

%% @doc insert column into matrix
-spec insert_column(pos_integer(), list(), #matrix{}) ->
                           #matrix{}.
insert_column(ColumnIndex, List, #matrix{
                                    column = Columns,
                                    row = Rows,
                                    unit = Unit,
                                    data = Bin,
                                    min = Min,
                                    max = Max
                                   } = Matrix) when is_list(List),
                                                    length(List) =:= Rows ->
    FixedColumnIndex = if
                           ColumnIndex =< 0 ->
                               0;
                           ColumnIndex =:= undefined;
                           ColumnIndex >= Columns ->
                               Columns;
                           true ->
                               ColumnIndex
                       end,
    NewBin = inner_insert_column(FixedColumnIndex, Columns, Unit, Bin, List),
    {ColumnMin, ColumnMax} = inner_min_max_of_lists(List),
    Matrix#matrix{
      column = Columns + 1,
      data = NewBin,
      min = min(Min, ColumnMin),
      max = max(Max, ColumnMax)
     };
insert_column(_, _, _) ->
    throw(not_match_rows).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc filter bitstring by function with index
-spec inner_filter_binary(pos_integer(), fun(), bitstring()) ->
                                 bitstring().
inner_filter_binary(Unit, Fun, BinData) ->
    inner_filter_binary(1, Unit, Fun, BinData).

inner_filter_binary(_, _, _, <<>>) ->
    <<>>;
inner_filter_binary(Index, Unit, Fun, BitString) ->
    <<V:Unit, Rest/bits>> = BitString,
    case Fun(Index) of
        true ->
            <<V:Unit, 
              (inner_filter_binary(Index + 1, Unit, Fun, Rest))/bits>>;
        false ->
            <<(inner_filter_binary(Index + 1, Unit, Fun, Rest))/bits>>
    end.

%% @doc zipwith for bitstring
-spec inner_zipwith(fun((integer(), integer()) -> integer()), 
                    pos_integer(), bitstring(), bitstring()) ->
                           bitstring().
inner_zipwith(_, _, <<>>, <<>>) ->
    <<>>;
inner_zipwith(Fun, Unit, BinA, BinB) ->
    <<A:Unit, ARest/bits>> = BinA,
    <<B:Unit, BRest/bits>> = BinB,
    <<(Fun(A, B)):Unit, (inner_zipwith(Fun, Unit, ARest, BRest))/bits>>.

-spec inner_all(fun ((integer()) -> boolean()), pos_integer(), #matrix{}) ->
                       boolean().
inner_all(_, _, <<>>) ->
    true;
inner_all(Fun, Unit, Bin) ->
    <<V:Unit, Rest/bits>> = Bin,
    case Fun(V) of
        true ->
            inner_all(Fun, Unit, Rest);
        false ->
            false
    end.

-spec inner_insert_column(InsertIndex :: pos_integer(),
                          Columns :: pos_integer(),
                          Unit :: pos_integer(), 
                          BinData :: bitstring(),
                          List :: list()) ->
                                 bitstring().
inner_insert_column(InsertIndex, Columns, Unit, BinData, List) ->
    inner_insert_column(InsertIndex, Columns, Unit, <<>>, BinData, 0, List).
inner_insert_column(_, _, _, PreData, LastData, _, []) ->
    <<PreData/bits, LastData/bits>>;
inner_insert_column(_, _, Unit, PreData, <<>>, _, [Last]) ->
    <<PreData/bits, Last:Unit>>;
inner_insert_column(IIndex, Columns, Unit, PreData, Data, Index,
                    [Head | Tail] = List) ->
    case Index =:= IIndex of
        true ->
            inner_insert_column(
              IIndex, Columns, Unit, <<PreData/bits, Head:Unit>>, Data, 
              inner_plus_columns(Index, Columns), Tail);
        false ->
            <<Value:Unit, Left/bits>> = Data,
            inner_insert_column(
              IIndex, Columns, Unit, <<PreData/bits, Value:Unit>>, Left,
              inner_plus_columns(Index, Columns), List)
    end.

inner_plus_columns(Index, Columns) ->
    Next = Index + 1,
    if
        Next > Columns ->
            0;
        true ->
            Next
    end.

-spec inner_min_max_of_lists(list()) ->
                                    {pos_integer(), pos_integer()}.
inner_min_max_of_lists([Head | Tail]) ->
    inner_min_max_of_lists(Head, Head, Tail).
inner_min_max_of_lists(Min, Max, []) ->
    {Min, Max};
inner_min_max_of_lists(Min, Max, [Head | Tail]) ->
    if
        Head < Min ->
            inner_min_max_of_lists(Head, Max, Tail);
        Head > Max ->
            inner_min_max_of_lists(Min, Head, Tail);
        true ->
            inner_min_max_of_lists(Min, Max, Tail)
    end.

-spec inner_min_max_of_binary(pos_integer(), bitstring()) ->
                                     {pos_integer(), pos_integer()}.
inner_min_max_of_binary(Unit, BinData) ->
    <<Value:Unit, Tail/bits>> = BinData,
    inner_min_max_of_binary(Value, Value, Unit, Tail).
inner_min_max_of_binary(Min, Max, _, <<>>) ->
    {Min, Max};
inner_min_max_of_binary(Min, Max, Unit, BinData) ->
    <<Head:Unit, Tail/bits>> = BinData,
    if
        Head < Min ->
            inner_min_max_of_binary(Head, Max, Unit, Tail);
        Head > Max ->
            inner_min_max_of_binary(Min, Head, Unit, Tail);
        true ->
            inner_min_max_of_binary(Min, Max, Unit, Tail)
    end.

