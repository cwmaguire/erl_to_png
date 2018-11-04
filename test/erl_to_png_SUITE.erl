-module(erl_to_png_SUITE).

-export([all/0]).
-export([test1/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [test1].

test1(_Config) ->
    ok.
