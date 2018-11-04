-module(erl_to_png_SUITE).

-export([all/0]).
-export([test_filter_tuples/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [test_filter_tuples].

test_filter_tuples(_Config) ->
    Filter = fun erl_to_png:filter_tuples/1,
    [] = lists:filter(Filter, [{undefined, <<>>, undefined}]),
    [] = lists:filter(Filter, [{undefined, not_a_bin, undefined}]),
    [] = lists:filter(Filter, [not_a_tuple]),
    [Tuple] = lists:filter(Filter, [Tuple = {undefined, <<"undefined">>, undefined}]).
