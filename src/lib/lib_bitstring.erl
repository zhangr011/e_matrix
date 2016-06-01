%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created : 25 May 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_bitstring).

%% API
-export([index/3, get/3, get_bits/3, update/4, foldl/4,
         zipwith/4, zipwith/5, zipfoldl/6]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc get the max value
-spec foldl(Fun :: fun((integer(), term()) -> term()),
            Init :: term(),
            Unit :: integer(),
            Bin :: bitstring()) ->
                   term().
foldl(_, Value, _, <<>>) ->
    Value;
foldl(Fun, In, Unit, Bin) ->
    <<Value:Unit, Tail/bits>> = Bin,
    foldl(Fun, Fun(Value, In), Unit, Tail).

%% @doc find the first index of target value
-spec index(Value :: integer(), 
            Unit :: pos_integer(), 
            Bin :: bitstring()) ->
                   false | pos_integer().
index(Value, Unit, Bin) ->
    inner_index(1, Value, Unit, Bin).

-spec get(Index :: pos_integer(),
          Unit :: pos_integer(),
          Bin :: bitstring()) ->
                 integer().
get(Index, Unit, Bin) ->
    Size = Unit * (Index - 1),
    <<_Pre:Size/bits, Value:Unit, _/bits>> = Bin,
    Value.

-spec get_bits(Index :: pos_integer(),
               Unit :: pos_integer(),
               Bin :: bitstring()) ->
                      bitstring().
get_bits(Index, Unit, Bin) ->
    Size = Unit * (Index - 1),
    <<_Pre:Size/bits, BinValue:Unit/bits, _/bits>> = Bin,
    BinValue.

-spec update(Index :: pos_integer(),
             Unit :: pos_integer(),
             Value :: integer(),
             Bin :: bitstring()) ->
                    bitstring().
update(Index, Unit, Value, Bin) ->
    Size = Unit * (Index - 1),
    <<Head:Size/bits, _:Unit/bits, Tail/bits>> = Bin,
    <<Head/bits, Value:Unit, Tail/bits>>.

%% @doc zipwith for bitstring
-spec zipwith(Fun :: fun((integer(), integer()) -> integer()), 
              UnitA :: pos_integer(),
              BinA :: bitstring(),
              UnitB :: pos_integer(), %% make sure UnitB >= UnitA
              BinB :: bitstring()) ->
                     bitstring().
zipwith(_, _, <<>>, _, <<>>) ->
    <<>>;
zipwith(Fun, UnitA, BinA, UnitB, BinB) ->
    <<A:UnitA, ARest/bits>> = BinA,
    <<B:UnitB, BRest/bits>> = BinB,
    <<(Fun(A, B)):UnitB, (zipwith(Fun, UnitA, ARest, UnitB, BRest))/bits>>.

-spec zipwith(Fun :: fun((integer(), integer()) -> integer()),
              Unit :: pos_integer(),
              BinA :: bitstring(),
              BinB :: bitstring()) ->
                     bitstring().
zipwith(Fun, Unit, BinA, BinB) ->
    zipwith(Fun, Unit, BinA, Unit, BinB).

%% @doc zipfoldl for bitstring
-spec zipfoldl(Fun :: fun((ValueA :: integer(), 
                           ValueB :: integer(),
                           InAcc :: term()) -> term()),
               Init :: term(),
               UnitA :: pos_integer(),
               BinA :: bitstring(),
               UnitB :: pos_integer(),
               BinB :: bitstring()) ->
                      term().
zipfoldl(_Fun, Value, _, <<>>, _, <<>>) ->
    Value;
zipfoldl(Fun, Init, UnitA, BinA, UnitB, BinB) ->
    <<ValueA:UnitA, TailA/bits>> = BinA,
    <<ValueB:UnitB, TailB/bits>> = BinB,
    zipfoldl(Fun, Fun(ValueA, ValueB, Init), UnitA, TailA, UnitB, TailB).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

-spec inner_index(Index :: pos_integer(),
                  Value :: integer(),
                  Unit :: pos_integer(),
                  Bin :: bitstring()) ->
                         false | pos_integer().
inner_index(_, _, _, <<>>) ->
    false;
inner_index(Index, Value, Unit, Bin) ->
    <<BinHead:Unit/bits, Tail/bits>> = Bin,
    if
        <<Value:Unit>> =:= BinHead ->
            Index;
        true ->
            inner_index(Index + 1, Value, Unit, Tail)
    end.

