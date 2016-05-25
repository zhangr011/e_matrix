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
         insert_column/3,
         
         hungarian_reduction/1
        ]).

-include("define_matrix.hrl").



%%%===================================================================
%%% API
%%%===================================================================

%% @doc 
-spec new(pos_integer(), pos_integer(), pos_integer(), list()) ->
                 #matrix{}.
new(Row, Column, Unit, List) when Column * Row =:= length(List) ->
    #matrix{
       column = Column,
       row = Row,
       unit = Unit,
       data = << <<D:Unit>> || D <- List >>
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
            <<_Head:PreIndex/bits, Result:RowLength/bits, _/bits>> = Data,
            Result
    end.

%% @doc add for all element in matrix
-spec add(integer(), #matrix{}) -> 
                 #matrix{};
         (#matrix{}, #matrix{}) ->
                 #matrix{}.
add(Scalar, #matrix{
               unit = Unit,
               data = Original
              } = Matrix) when is_integer(Scalar) ->
    Matrix#matrix{
      data = << <<(X + Scalar):Unit>> || <<X:Unit>> <= Original>>
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
                data = Original
               } = Matrix) when is_integer(Scalar) ->
    Matrix#matrix{
      data = <<<<(X * Scalar):Unit>> || <<X:Unit>> <= Original>>
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
    Matrix#matrix{
      data = inner_zipwith(Fun, Unit, BinDataA, BinDataB)
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
                              data = Bin
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
                     <<Head:HeadLength/bits, Rest/bits>> = Bin,
                     <<Head/bits, InsertBin/bits, Rest/bits>>
             end,
    Matrix#matrix{
      row = Rows + 1,
      data = NewBin
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
                                    data = Bin
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
    Matrix#matrix{
      column = Columns + 1,
      data = NewBin
     };
insert_column(_, _, _) ->
    throw(not_match_rows).

%% @doc reduction matrix for hungarian algorithm, that is
%% 1 2  3    0 1 2    0 0 0      2 1 1    1 0 0    - - 3
%% 4 6  8 -> 0 2 4 -> 0 1 2 ( -> 1 1 2 -> 0 0 1 -> - 6 - )
%% 7 10 13   0 3 6    0 2 4      1 2 4    0 1 2    7 - -
%% for more: https://www.topcoder.com/community/data-science/data-science-tutorials/assignment-problem-and-hungarian-algorithm/
-spec hungarian_reduction(Matrix :: #matrix{}) ->
                                 #matrix{}.
hungarian_reduction(#matrix{
                       row = Same,
                       column = Same
                      } = Matrix) ->
    RowReduction = inner_row_hungarian_reduction(Matrix),
    inner_column_hungarian_reduction(RowReduction);
hungarian_reduction(_) ->
    throw(not_square_matrix).

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
            <<Value:Unit/bits, Left/bits>> = Data,
            inner_insert_column(
              IIndex, Columns, Unit, <<PreData/bits, Value/bits>>, Left,
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

%% @doc row reduction for hungarian
%% 1 2 3    0 1 2
%% 3 1 5 -> 2 0 4
%% 1 4 7    0 3 6
-spec inner_row_hungarian_reduction(#matrix{}) ->
                                           #matrix{}.
inner_row_hungarian_reduction(#matrix{
                                 column = Column,
                                 unit = Unit,
                                 data = Bin
                                } = Matrix) ->
    Matrix#matrix{
      data = inner_row_hungarian_reduction(Column, Unit, Bin)
     }.

inner_row_hungarian_reduction(Unit, Bin) ->
    %% find the min at first
    Min = inner_min(Unit, Bin),
    %% all minus the minimum
    <<<<(Value - Min):Unit>> || <<Value:Unit>> <= Bin>>.

inner_row_hungarian_reduction(_, _, <<>>) ->
    <<>>;
inner_row_hungarian_reduction(Column, Unit, Bin) ->
    Size = Unit * Column,
    <<BinRow:Size/bits, Tail/bits>> = Bin,
    <<(inner_row_hungarian_reduction(Unit, BinRow))/bits, 
      (inner_row_hungarian_reduction(Column, Unit, Tail))/bits>>.

%% @doc column reduction for hungarian
%% 0 1 2    0 1 0
%% 2 0 4 -> 2 0 2
%% 0 3 2    0 3 0
-spec inner_column_hungarian_reduction(#matrix{}) ->
                                              #matrix{}.
inner_column_hungarian_reduction(#matrix{
                                    column = Column,
                                    unit = Unit,
                                    data = Bin
                                   } = Matrix) ->
    Matrix#matrix{
      data = inner_column_hungarian_reduction(Column, Unit, Bin)
     }.

inner_column_hungarian_reduction(Column, Unit, Bin) ->
    BinMin = inner_min(Column, Unit, Bin),
    Size = Unit * Column,
    <<<<(inner_zipwith(fun (ValueA, ValueB) -> 
                               ValueA - ValueB
                       end, Unit, BinRow, BinMin))/bits>> 
      || <<BinRow:Size/bits>> <= Bin>>.

%% -spec inner_min_max_of_lists(list()) ->
%%                                     {pos_integer(), pos_integer()}.
%% inner_min_max_of_lists([Head | Tail]) ->
%%     inner_min_max_of_lists(Head, Head, Tail).
%% inner_min_max_of_lists(Min, Max, []) ->
%%     {Min, Max};
%% inner_min_max_of_lists(Min, Max, [Head | Tail]) ->
%%     if
%%         Head < Min ->
%%             inner_min_max_of_lists(Head, Max, Tail);
%%         Head > Max ->
%%             inner_min_max_of_lists(Min, Head, Tail);
%%         true ->
%%             inner_min_max_of_lists(Min, Max, Tail)
%%     end.

-spec inner_min(Column :: pos_integer(), Unit :: pos_integer(),
                Bin :: bitstring()) ->
                       bitstring().
inner_min(Column, Unit, Bin) ->
    Size = Unit * Column,
    <<BinMin:Size/bits, Tail/bits>> = Bin,
    inner_min(Column, Unit, BinMin, Tail).

inner_min(_Column, _Unit, BinMin, <<>>) ->
    BinMin;
inner_min(Column, Unit, BinMin, Bin) ->
    Size = Unit * Column,
    <<BinMin2:Size/bits, Tail/bits>> = Bin,
    inner_min(
      Column, Unit, 
      inner_zipwith(fun erlang:min/2, Unit, BinMin, BinMin2), Tail).

-spec inner_min(Unit :: pos_integer(), BinData :: bitstring()) ->
                       pos_integer().
inner_min(Unit, BinData) ->
    <<Value:Unit, Tail/bits>> = BinData,
    inner_min2(Value, Unit, Tail).

inner_min2(Min, _, <<>>) ->
    Min;
inner_min2(Min, Unit, BinData) ->
    <<Head:Unit, Tail/bits>> = BinData,
    if
        Head < Min ->
            inner_min2(Head, Unit, Tail);
        true ->
            inner_min2(Min, Unit, Tail)
    end.

