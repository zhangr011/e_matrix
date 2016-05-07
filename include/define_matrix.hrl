-ifndef(DEFINE_E_MATRIX_HRL).
-define(DEFINE_E_MATRIX_HRL, true).

-define(INT1,   1).  %% bits of int1, for: 0, 1
-define(INT8,   8).  %% bits of int8
-define(INT16, 16).  %% bits of int16
-define(INT32, 32).  %% bits of int32
-define(INT64, 64).  %% bits of int64
-define(FLOAT, 64).  %% bits of float

-record(matrix, {
          column :: pos_integer(), %% column number
          row :: pos_integer(),    %% row number
          unit :: pos_integer(),   %% ?INT1 | ?INT8 | ?INT16 | ?INT32 | ?INT64
          %% for column: 4 row: 2 size: ?INT1
          %% <<0, 1, 0, 0, 1, 0, 0, 0>>
          data :: bitstring() 
         }).

-endif.

