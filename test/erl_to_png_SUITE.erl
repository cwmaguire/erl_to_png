-module(erl_to_png_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([test_filter_tuples/1]).
-export([test_render_tuples/1]).
-export([test_lines/1]).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    os:putenv("NIF_PATH", "../../deps/erl_freetype/ebin/render_nif"),
    Config.

end_per_suite(_Config) ->
    os:putenv("NIF_DIR", ""),
    ok.

all() ->
    [test_filter_tuples,
     test_render_tuples,
     test_lines].

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
    [{1, R1 = {Bin1, W, H, Top}, colour_1}] =
        erl_to_png:render_tuples([{1, <<"a">>, colour_1}]),
    true = is_binary(Bin1),
    true = is_integer(W),
    true = is_integer(H),
    true = is_integer(Top),
    [T1, T2] = erl_to_png:render_tuples([{1, <<"a">>, colour_1},
                                         {2, <<"b">>, colour_2}]),
    {1, R1, colour_1} = T1,
    {2, {Bin2, W2, H2, Top2}, colour_2} = T2,
    true = is_binary(Bin2),
    true = Bin1 /= Bin2,
    true = 100 < size(Bin1),
    true = 100 < size(Bin2),
    true = is_integer(W2),
    true = is_integer(H2),
    true = is_integer(Top2).

test_lines(_Config) ->
    U = undefined,
    N = {0, U, U},
    A = {1, U, U},
    [[A, A]] = erl_to_png:lines([A, A]),
    B = {2, U, U},
    [[A], [B]] = erl_to_png:lines([A, B]),
    [[A, A], [B, B]] = erl_to_png:lines([A, A, B, B]),
    NoLine = {noline, U, U},
    %% No line gets set to 0 if it's the very first line
    [[N], [A]] = erl_to_png:lines([NoLine, A]),
    %% 'noline' lines are subsumed by the current line
    [[A, A, A], [B, B]] = erl_to_png:lines([A, NoLine, A, B, B]),
    %% No going backwards: if a line number is out of ordered it's
    %% re-numbed to match the current line
    [[A], [B, B, B, B]] = erl_to_png:lines([A, B, A, NoLine, B]).
