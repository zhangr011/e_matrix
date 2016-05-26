-ifndef(DEFINE_HANGARIAN_HRL).
-define(DEFINE_HANGARIAN_HRL, true).

-include("define_matrix.hrl").
-include("define_queue.hrl").

-define(N, 55).                    %% max number of vertices inf one part
-define(MINUS_ONE,  -1).

-define(BIN_MINUS_ONE_INT8, <<-1:?INT8>>).
-define(BIN_MINUS_TWO_INT8, <<-2:?INT8>>).

-record(hungarian_helper, {
          unit :: pos_integer(),   %% unit of matrix
          cost :: #matrix{},       %% cost matrix
          n :: integer(),          %% n workers and n jobs
          max_match :: integer(),  %% match numbers
          lx :: bitstring(),       %% labels of X parts
          ly :: bitstring(),       %% labels of Y parts
          xy :: bitstring(),       %% xy[x] - vertex that is matched with x
          yx :: bitstring(),       %% yx[y] - vertex that is matched with y
          source :: bitstring(),   %% sets for source
          target :: bitstring(),   %% sets for target
          slack :: bitstring(),    %% for slack
          slackx :: bitstring(),   %% slackx[y] such a vertex, that
          %% l(slackx[y]) + l(y) - w(slackx[y],y) = slack[y]
          prev :: bitstring(),     %% array for memorizing alternating paths
          %% queue for bfs wr, rd - write and read pos in queue 
          queue :: #wr_queue{}
         }).

-endif.

