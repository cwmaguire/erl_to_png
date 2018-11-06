-module(erl_to_png_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([test_filter_tuples/1]).
-export([test_render_tuples/1]).
-export([test_lines/1]).
-export([test_combine_scanlines/1]).

-include_lib("common_test/include/ct.hrl").

-define(E2P, erl_to_png).

init_per_suite(Config) ->
    os:putenv("NIF_PATH", "../../deps/erl_freetype/ebin/render_nif"),
    Config.

end_per_suite(_Config) ->
    os:putenv("NIF_DIR", ""),
    ok.

all() ->
    [test_filter_tuples,
     test_render_tuples,
     test_lines,
     test_combine_scanlines].

test_filter_tuples(_Config) ->
    Filter = fun ?E2P:filter_tuples/1,
    [] = lists:filter(Filter, [{undefined, <<>>, undefined}]),
    [] = lists:filter(Filter, [{undefined, not_a_bin, undefined}]),
    [] = lists:filter(Filter, [not_a_tuple]),
    [Tuple] = lists:filter(Filter, [Tuple = {undefined,
                                             <<"undefined">>,
                                             undefined}]).

test_render_tuples(_Config) ->
    [] = ?E2P:render_tuples([{undefined, <<>>, undefined}]),
    [{1, R1 = {Bin1, W, H, Top}, colour_1}] =
        ?E2P:render_tuples([{1, <<"a">>, colour_1}]),
    true = is_binary(Bin1),
    true = is_integer(W),
    true = is_integer(H),
    true = is_integer(Top),
    [T1, T2] = ?E2P:render_tuples([{1, <<"a">>, colour_1},
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
    [[A, A]] = ?E2P:lines([A, A]),
    B = {2, U, U},
    [[A], [B]] = ?E2P:lines([A, B]),
    [[A, A], [B, B]] = ?E2P:lines([A, A, B, B]),
    NoLine = {noline, U, U},
    %% No line gets set to 0 if it's the very first line
    [[N], [A]] = ?E2P:lines([NoLine, A]),
    %% 'noline' lines are subsumed by the current line
    [[A, A, A], [B, B]] = ?E2P:lines([A, NoLine, A, B, B]),
    %% No going backwards: if a line number is out of ordered it's
    %% re-numbed to match the current line
    [[A], [B, B, B, B]] = ?E2P:lines([A, B, A, NoLine, B]).

%% A letter consists of a list of scanlines.
%% Combining letters means combining a list of letters,
%% which is a list of lists of scanlines, which is a
%% list of lists of lists of pixels. Phew!
test_combine_scanlines(_Config) ->
    ScanlineA1 = [1, 2], % list of pixels
    ScanlineA2 = [3, 4],
    LetterA = [ScanlineA1, ScanlineA2], % list' of pixels

    ScanlineB1 = [5, 6],
    ScanlineB2 = [7, 8],
    LetterB = [ScanlineB1, ScanlineB2],

    Letters = [LetterA, LetterB], % list'' of pixels

    Scanline1 = ScanlineA1 ++ ScanlineB1,
    Scanline2 = ScanlineA2 ++ ScanlineB2,
    Scanlines = [Scanline1, Scanline2],

    Scanlines = ?E2P:combine_scanlines(Letters).
