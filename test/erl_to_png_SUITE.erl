-module(erl_to_png_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([test_filter_tuples/1]).
-export([test_render_tuples/1]).
-export([test_lines/1]).
-export([test_combine_scanlines/1]).
-export([test_blank_pixels/1]).
-export([test_pad_length/1]).
-export([test_letter_to_scanlines/1]).
-export([test_pixels/1]).
-export([test_letters_to_scanlines/1]).
-export([test_lines_to_scanlines/1]).
-export([test_scanlines/1]).

-include_lib("common_test/include/ct.hrl").
-include("png.hrl").

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
     test_combine_scanlines,
     test_blank_pixels,
     test_pad_length,
     test_letter_to_scanlines,
     test_pixels,
     test_letters_to_scanlines,
     test_lines_to_scanlines,
     test_scanlines].

test_filter_tuples(_Config) ->
    Filter = fun ?E2P:filter_tuples/1,
    [] = lists:filter(Filter, [{undefined, <<>>, undefined}]),
    [] = lists:filter(Filter, [{undefined, not_a_bin, undefined}]),
    [] = lists:filter(Filter, [not_a_tuple]),
    [Tuple] = lists:filter(Filter, [Tuple = {undefined,
                                             <<"undefined">>,
                                             undefined}]).

test_render_tuples(_Config) ->
    FontPath = "/System/Library/Fonts/Menlo.ttc",
    FontSize = 12,
    Font = {FontPath, FontSize},
    [] = ?E2P:render_tuples(Font, [{undefined, <<>>, undefined}]),
    [{1, R1 = {Bin1, W, H, Top}, colour_1}] =
        ?E2P:render_tuples(Font, [{1, <<"a">>, colour_1}]),
    true = is_binary(Bin1),
    true = is_integer(W),
    true = is_integer(H),
    true = is_integer(Top),
    [T1, T2] = ?E2P:render_tuples(Font,
                                  [{1, <<"a">>, colour_1},
                                   {2, <<"b">>, colour_2}]),
    {1, R1, colour_1} = T1,
    {2, {Bin2, W2, H2, Top2}, colour_2} = T2,
    true = is_binary(Bin2),
    true = Bin1 /= Bin2,
    true = 50 < size(Bin1),
    true = 50 < size(Bin2),
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

test_blank_pixels(_Config) ->
    B = _BlankPixel = #px{r = 0, g = 0, b = 0, a = 255},
    [] = ?E2P:blank_pixels(0),
    [B] = ?E2P:blank_pixels(1),
    [B, B] = ?E2P:blank_pixels(2).

test_pad_length(_Config) ->
    A = _Pixel = #px{r = 1, g = 2, b = 3, a = 255},
    B = _BlankPixel = #px{r = 0, g = 0, b = 0, a = 255},
    [] = ?E2P:pad_length([], 0),
    [B, B] = ?E2P:pad_length([], 2),
    [A, B] = ?E2P:pad_length([A], 2),
    [A, B, B] = ?E2P:pad_length([A], 3),
    [A] = ?E2P:pad_length([A], 1).


test_letter_to_scanlines(_Config) ->
    [<<0, 1>>, <<2, 3>>] = ?E2P:letter_to_scanlines(<<0, 1, 2, 3>>, 2).

test_pixels(_Config) ->
    Px1 = #px{r = 1, g = 2, b = 3, a = 4},
    Px2 = Px1#px{a = 5},
    [Px1, Px2] = ?E2P:pixels(<<4, 5>>, {1, 2, 3}).

test_letters_to_scanlines(_Config) ->
    U = undefined,
    Bin1 = <<1, 2, 3, 4, 5, 6>>,
    C1 = {1, 2, 3},
    L1 = {U, {Bin1, 3, 2, 1}, C1},
    Bin2 = <<7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18>>,
    C2 = {4, 5, 6},
    L2 = {U, {Bin2, 3, 4, 2}, C2},

    Px0 = _BlankPixel = px(0, 0, 0, 255),
    BlankLine = [Px0, Px0, Px0],
    [Px1, Px2, Px3, Px4, Px5, Px6] = [px(C1, A) || A <- b2l(Bin1)],
    Scanlines1 = [BlankLine,
                 [Px1, Px2, Px3],
                 [Px4, Px5, Px6],
                 BlankLine],
    Pxs2 = [px(C2, A) || A <- b2l(Bin2)],
    Pxs2_1 = lists:sublist(Pxs2, 1, 3),
    Pxs2_2 = lists:sublist(Pxs2, 4, 3),
    Pxs2_3 = lists:sublist(Pxs2, 7, 3),
    Pxs2_4 = lists:sublist(Pxs2, 10, 3),
    Scanlines2 = [Pxs2_1,
                 Pxs2_2,
                 Pxs2_3,
                 Pxs2_4],
    ScanlineList = [Scanlines1, Scanlines2],
    ScanlineList = ?E2P:letters_to_scanlines([L1, L2], 2, 2).

test_lines_to_scanlines(_Config) ->
    U = undefined,
    Bin1 = <<1, 2, 3, 4, 5, 6>>,
    C1 = {1, 2, 3},
    L1 = {U, {Bin1, 3, 2, 1}, C1},

    Bin2 = <<7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18>>,
    C2 = {4, 5, 6},
    L2 = {U, {Bin2, 3, 4, 2}, C2},

    Bin3 = <<19, 20>>,
    C3 = {7, 8, 8},
    L3 = {U, {Bin3, 2, 1, 1}, C3},

    Bin4 = <<22, 23, 24, 25, 26, 27>>,
    C4 = {10, 11, 12},
    L4 = {U, {Bin4, 2, 3, 2}, C4},

    Lines = [[L1, L2], [L3, L4]],

    Px0 = _BlankPixel = px(0, 0, 0, 255),

    [Px1_1, Px1_2, Px1_3,
     Px1_4, Px1_5, Px1_6] = [px(C1, A) || A <- b2l(Bin1)],

    [Px2_1,  Px2_2,  Px2_3,
     Px2_4,  Px2_5,  Px2_6,
     Px2_7,  Px2_8,  Px2_9,
     Px2_10, Px2_11, Px2_12] = [px(C2, A) || A <- b2l(Bin2)],

    [Px3_1, Px3_2] = [px(C3, A) || A <- b2l(Bin3)],

    [Px4_1, Px4_2,
     Px4_3, Px4_4,
     Px4_5, Px4_6] = [px(C4, A) || A <- b2l(Bin4)],

    [[Px0,   Px0,   Px0,   Px2_1,  Px2_2,  Px2_3],
     [Px1_1, Px1_2, Px1_3, Px2_4,  Px2_5,  Px2_6],
     [Px1_4, Px1_5, Px1_6, Px2_7,  Px2_8,  Px2_9],
     [Px0,   Px0,   Px0,   Px2_10, Px2_11, Px2_12],
     [Px0,   Px0,   Px4_1, Px4_2],
     [Px3_1, Px3_2, Px4_3, Px4_4],
     [Px0,   Px0,   Px4_5, Px4_6],
     [Px0,   Px0,   Px0,   Px0]] =
        ?E2P:lines_to_scanlines(Lines, 2, 2).

test_scanlines(_Config) ->
    U = undefined,
    Bin1 = <<1, 2, 3, 4, 5, 6>>,
    C1 = {1, 2, 3},
    L1 = {U, {Bin1, 3, 2, 1}, C1},

    Bin2 = <<7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18>>,
    C2 = {4, 5, 6},
    L2 = {U, {Bin2, 3, 4, 2}, C2},

    Bin3 = <<19, 20>>,
    C3 = {7, 8, 8},
    L3 = {U, {Bin3, 2, 1, 1}, C3},

    Bin4 = <<22, 23, 24, 25, 26, 27>>,
    C4 = {10, 11, 12},
    L4 = {U, {Bin4, 2, 3, 2}, C4},

    Lines = [[L1, L2], [L3, L4]],

    [[Px0,   Px0,   Px0,   Px2_1,  Px2_2,  Px2_3],
     [Px1_1, Px1_2, Px1_3, Px2_4,  Px2_5,  Px2_6],
     [Px1_4, Px1_5, Px1_6, Px2_7,  Px2_8,  Px2_9],
     [Px0,   Px0,   Px0,   Px2_10, Px2_11, Px2_12],
     [Px0,   Px0,   Px4_1, Px4_2,  Px0,    Px0],
     [Px3_1, Px3_2, Px4_3, Px4_4,  Px0,    Px0],
     [Px0,   Px0,   Px4_5, Px4_6,  Px0,    Px0],
     [Px0,   Px0,   Px0,   Px0,    Px0,    Px0]] =
        ?E2P:scanlines(Lines, 2, 2).

px({R, G, B}, A) ->
    px(R, G, B, A).

%px(R, G, B) ->
    %px(R, G, B, 255).

px(R, G, B, A) ->
    #px{r = R, g = G, b = B, a = A}.

b2l(Bin) ->
    binary_to_list(Bin).
