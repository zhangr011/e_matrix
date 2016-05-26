%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_hungarian_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

-include("define_matrix.hrl").
-include("define_queue.hrl").

hungarian_test_() ->
    Cost = lib_matrix:new(3, 3, ?INT16, [7, 4, 3, 3, 1, 2, 3, 0, 0]),
    List = lib_hungarian:hungarian(Cost),
    [?_assertEqual(List, [])].

