-ifndef(DEFINE_QUEUE_HRL).
-define(DEFINE_QUEUE_HRL, true).

-record(wr_queue, {
          unit :: pos_integer(),
          data :: bitstring(),
          wr :: non_neg_integer(),
          rd :: non_neg_integer()
         }).

-endif.

