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
         zipwith/3
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
            inner_filter_binary(
              Unit,
              fun (Index) when
                        Index > (RowIndex - 1) * Columns,
                        Index =< RowIndex * Columns ->
                      true;
                  (_) ->
                      false
              end, Data)
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

%% @doc iterate matrix
-spec zipwith(fun(), #matrix{}, #matrix{}) ->
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
-spec inner_zipwith(fun(), pos_integer(), bitstring(), bitstring()) ->
                           bitstring().
inner_zipwith(_, _, <<>>, <<>>) ->
    <<>>;
inner_zipwith(Fun, Unit, BinA, BinB) ->
    <<A:Unit, ARest/bits>> = BinA,
    <<B:Unit, BRest/bits>> = BinB,
    <<(Fun(A, B)):Unit, (inner_zipwith(Fun, Unit, ARest, BRest))/bits>>.

