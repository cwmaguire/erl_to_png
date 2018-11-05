-module(erl_to_png_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([test_filter_tuples/1]).
-export([test_render_tuples/1]).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    os:putenv("NIF_PATH", "../../deps/erl_freetype/ebin/render_nif"),
    Config.

end_per_suite(_Config) ->
    os:putenv("NIF_DIR", ""),
    ok.

all() ->
    [test_filter_tuples,
     test_render_tuples].

test_filter_tuples(_Config) ->
    Filter = fun erl_to_png:filter_tuples/1,
    [] = lists:filter(Filter, [{undefined, <<>>, undefined}]),
    [] = lists:filter(Filter, [{undefined, not_a_bin, undefined}]),
    [] = lists:filter(Filter, [not_a_tuple]),
    [Tuple] = lists:filter(Filter, [Tuple = {undefined,
                                             <<"undefined">>,
                                             undefined}]).

test_render_tuples(_Config) ->
    [] = erl_to_png:render_tuples([{undefined, <<>>, undefined}]),
    [{1, Bin1, colour_1}] = erl_to_png:render_tuples([{1, <<"a">>, colour_1}]),
    true = is_binary(Bin1),
    [T1, T2] = erl_to_png:render_tuples([{1, <<"a">>, colour_1},
                                         {2, <<"b">>, colour_2}]),
    {1, Bin1, colour_1} = T1,
    {2, Bin2, colour_2} = T2,
    true = is_binary(Bin2),
    Bin1 /= Bin2,
    100 < size(Bin1),
    100 < size(Bin2).
