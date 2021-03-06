-module(erl_to_png).

-include("png.hrl").

-export([render/2]).
-export([render/3]).

% Test exports
-export([is_valid_tuple/1]).
-export([render_tuples/2]).
-export([render_tuples/3]).
-export([lines/1]).
-export([lines/2]).
-export([scanlines/3]).
-export([pad_length/2]).
-export([max_top/1]).
-export([max_bottom/1]).
-export([max_width/1]).
-export([longest_scanline/1]).
-export([combine_scanlines/1]).
-export([blank_pixels/1]).
-export([letter_to_scanlines/2]).
-export([letters_to_scanlines/3]).
-export([lines_to_scanlines/3]).
-export([pixels/2]).
-export([monospace_letter/2]).
-export([monospace_letter/5]).

-spec render(list, {list, integer}) -> ok.
render(Filename, Font) ->
    render(Filename, Font, []).

render(Filename, Font, IncludePaths) ->
    Tuples0 = erl_to_tuples:get_tuples(Filename, IncludePaths),
    Tuples = lists:filter(fun is_valid_tuple/1, Tuples0),
    Letters = render_tuples(Font, Tuples),
    %MaxHeight = max_height(Letters),
    MaxTop = max_top(Letters),
    io:format(user, "MaxTop = ~p~n", [MaxTop]),
    MaxBottom = max_bottom(Letters),
    io:format(user, "MaxBottom = ~p~n", [MaxBottom]),
    MaxWidth = max_width(Letters),
    io:format(user, "MaxWidth = ~p~n", [MaxWidth]),
    MonospacedLetters = [monospace_letter(L, MaxWidth) || L <- Letters],
    Lines = lines(MonospacedLetters),
    io:format(user, "Lines ~p~n", [length(Lines)]),
    Scanlines0  = scanlines(Lines, MaxTop, MaxBottom),
    io:format(user, "Scanlines0 ~p~n", [length(Scanlines0)]),
    Longest = longest_scanline(Scanlines0),
    MaxLength = length(Longest),
    %% TODO Why are we padding here _and_ in scanlines?
    Scanlines = [pad_length(SL, MaxLength) || SL <- Scanlines0],
    io:format("Scanlines: ~p~n", [length(Scanlines)]),
    Png = png(Scanlines, MaxLength),
    png:write_scanlines(Png, Filename ++ ".png").

is_valid_tuple(Tuple = {_, <<>>, _}) ->
    io:format("Filter out tuple with empty bin: ~p~n", [Tuple]),
    false;
is_valid_tuple(Tuple = {_, Bin, _}) when is_binary(Bin) and size(Bin) == 0 ->
    io:format("Filter out tuple with size zero bin: ~p~n", [Tuple]),
    false;
is_valid_tuple(NotTuple) when not(is_tuple(NotTuple)) ->
    io:format("Filter out non-tuple: ~p~n", [NotTuple]),
    false;
is_valid_tuple({_, NotBin, _}) when not(is_binary(NotBin)) ->
    io:format("Filter out non-binary: ~p~n", [NotBin]),
    false;
is_valid_tuple(_) ->
    true.

render_tuples(Font, Tuples) ->
    render_tuples(Font, Tuples, []).

render_tuples(_, [], Letters) ->
    lists:reverse(Letters);
render_tuples(Font, [{_, <<>>, _} | Tuples], Letters) ->
    render_tuples(Font, Tuples, Letters);
render_tuples(Font,
              [{Line, <<Char:1/binary, Bin/binary>>, Colour} | Tuples],
              Letters) ->
    Letter = render_letter(Char, Font),

    render_tuples(Font,
                  [{Line, Bin, Colour} | Tuples],
                  [{Line, Letter, Colour} | Letters]);
render_tuples(Font,
              [{Line, <<Char:1/binary>>, Colour} | Tuples],
              Letters) ->
    Letter = render_letter(Char, Font),
    render_tuples(Font,
                  Tuples,
                  [{Line, Letter, Colour} | Letters]);
render_tuples(Font, [BadTuple = {_, Bin, _} | Tuples], Letters) ->
    io:format("Skipping bad tuple: ~p with bin size ~p~n", [BadTuple, size(Bin)]),
    render_tuples(Font, Tuples, Letters).

%draw_pixmap(<<>>, _, _) ->
%    ok;
%draw_pixmap(<<Char/integer, Rest/binary>>, W, Count) ->
%    io:format("~2.. B", [floor(Char / 2.6)]),
%    case Count of
%        X when X rem W == 0 ->
%            io:format("~n");
%        _ ->
%            ok
%    end,
%    draw_pixmap(Rest, W, Count + 1).

render_letter(Char, {FontPath, FontSize}) ->
    Letter = render_char:render_char(binary_to_list(Char),
                                     length(FontPath),
                                     FontPath,
                                     FontSize),
    {Render, W, H, T, BinWidth} = Letter,
    %io:format("Rendered ~p on line ~p with colour ~p."
              %" Bin size: ~p, "
              %"W: ~p, H: ~p, T: ~p, BW: ~p~n",
              %[Char, Line, Colour, size(Render),
               %W, H, T, BinWidth]),
    Extract = extract_letter(Render, W, H, BinWidth),
    %io:format("Rendered ~p on line ~p with colour ~p."
              %" Bin size: ~p, Letter size: ~p, "
              %"W: ~p, H: ~p, T: ~p, BW: ~p~n",
              %[Char, Line, Colour, size(Render),
               %size(Extract), W, H, T, BinWidth]),
    {Extract, W, H, T}.

extract_letter(Bin, LetterW, LetterH, BinW) ->
    extract_letter(Bin, LetterW, LetterH, BinW, <<>>, 0).

extract_letter(_, _, H, _, ExtractBin, H = _Count) ->
    ExtractBin;
extract_letter(Bin, LetterW, LetterH, BinW, ExtractBin, Count) ->
    %io:format("Extracting ~p x ~p line #~p from ~p byte binary "
              %"with width ~p~n",
              %[LetterW, LetterH, Count + 1, size(Bin), BinW]),
    <<ExtractLine:BinW/binary, Rest/binary>> = Bin,
    <<LetterLine:LetterW/binary, _/binary>> = ExtractLine,
    extract_letter(Rest,
                   LetterW,
                   LetterH,
                   BinW,
                   <<ExtractBin/binary, LetterLine/binary>>,
                   Count + 1).

monospace_letter({Line, Letter, Colour}, MaxW) ->
    Monospaced = monospace_letter(Letter, MaxW),
    {Line, Monospaced, Colour};
monospace_letter({Bin, W, H, T}, MaxW) ->
    Pad = MaxW - W,
    PadR = Pad div 2,
    PadL = Pad - PadR,
    BinPadR = <<0:(8 * PadR)>>,
    BinPadL = <<0:(8 * PadL)>>,
    MonospacedLetter = monospace_letter(Bin, W, BinPadL, BinPadR, <<>>),
    {MonospacedLetter, MaxW, H, T}.

monospace_letter(<<>>, _, _, _, PaddedLetter) ->
    PaddedLetter;
monospace_letter(Bin, W, PadL, PadR, PaddedLetter) ->
    <<Line:W/binary, Rest/binary>> = Bin,
    Padded = <<PaddedLetter/binary, PadL/binary, Line/binary, PadR/binary>>,
    monospace_letter(Rest, W, PadL, PadR, Padded).

lines([]) ->
    [];
lines([{noline, X, Y} | Letters]) ->
    lines(Letters, [[{0, X, Y}]]);
lines([Letter | Letters]) ->
    lines(Letters, [[Letter]]).

lines([], Lines) ->
    lists:reverse([lists:reverse(L) || L <- Lines]);
% Ignore line numbers that are out of order and lines
% that are 'noline'
lines([Letter = {LineNo, _, _} | Letters],
      Lines = [[{HigherLineNo, _, _} | _] | _])
    when is_integer(LineNo),
         is_integer(HigherLineNo),
         LineNo > HigherLineNo ->
    lines(Letters, [[Letter] | Lines]);
% FIXME include files get included in the source and
%       have their own line numbers, then the main file
%       restarts at 0
% Replace the out-of-place line number with the current
% line number since it isn't an int and greater than the
% current line number
lines([{_, X, Y} | Letters],
      [Line = [{LineNo, _, _} | _] | Lines]) ->
    lines(Letters, [[{LineNo, X, Y} | Line] | Lines]).

scanlines(Lines0, MaxT, MaxB) ->
    Lines = lines_to_scanlines(Lines0, MaxT, MaxB),
    io:format(user, "Scanlines size: ~p~n", [length(Lines)]),

    MaxLength = lists:max([length(L) || L <- Lines]),
    io:format(user, "MaxLength = ~p~n", [MaxLength]),
    _EqualLines = [pad_length(L, MaxLength) || L <- Lines].

%max_length(CharLines) ->
    %MaxCharLineLengths = [max_line_length(CL) || CL <- CharLines],
    %lists:max(MaxCharLineLengths).

%max_line_length(CharLine) ->
    %lists:max([length(Scanline) || Scanline <- CharLine]).

lines_to_scanlines(Lines, MaxT, MaxB) ->
    lines_to_scanlines(Lines, MaxT, MaxB, []).

lines_to_scanlines([], _, _, Scanlines) ->
    Scanlines;
lines_to_scanlines([Line | Lines], MaxT, MaxB, Scanlines) ->
    Letters = letters_to_scanlines(Line, MaxT, MaxB),
    io:format(user, "Letters size: ~p~n", [length(Letters)]),
    LetterScanlines = combine_scanlines(Letters),
    StackedScanlines = Scanlines ++ LetterScanlines,
    lines_to_scanlines(Lines, MaxT, MaxB, StackedScanlines).

letters_to_scanlines(Letters, MaxT, MaxB) ->
    lists:reverse(letters_to_scanlines(Letters, MaxT, MaxB, [])).

letters_to_scanlines([], _, _, Scanlines) ->
    Scanlines;
letters_to_scanlines([Letter | Letters], MaxT, MaxB, Scanlines) ->
    {_, {Bin, W, H, T}, Colour} = Letter,
    Scanlines0 = letter_to_scanlines(Bin, W),
    Pixels = pixels(Scanlines0, Colour),
    %draw_scanlines(Pixels),
    BlankLine = blank_pixels(W),
    TopPadding = lists:duplicate(MaxT - T, BlankLine),
    BottomPadding = lists:duplicate(MaxB - (H - T), BlankLine),
    LetterScanlines = TopPadding ++ Pixels ++ BottomPadding,
    letters_to_scanlines(Letters, MaxT, MaxB, [LetterScanlines | Scanlines]).

pixels(Scanlines, RGB) when is_list(Scanlines) ->
    [pixels(S, RGB) || S <- Scanlines];
pixels(Scanline, {R, G, B}) when is_binary(Scanline) ->
    Alphas = binary_to_list(Scanline),
    [#px{r = R, g = G, b = B, a = A} || A <- Alphas].

%draw_scanlines(Scanlines) ->
%    [draw_pixels(S) || S <- Scanlines].

%draw_pixels(Scanline) ->
    %[draw_pixel(Px) || Px <- Scanline],
    %io:format("~n").

%draw_pixels(Scanline) ->
%    [draw_pixel_rg(Px) || Px <- Scanline],
%    io:format("~n"),
%    [draw_pixel_ba(Px) || Px <- Scanline],
%    io:format("~n").

%draw_pixel_rg(Px) ->
%    ARatio = Px#px.a / 255,
%    R = floor(Px#px.r / 255 * 99 * ARatio),
%    G = floor(Px#px.g / 255 * 99 * ARatio),
%    io:format("~2.. B~2.. B",
%              [R, G]).

%draw_pixel_ba(Px) ->
%    ARatio = Px#px.a / 255,
%    B = floor(Px#px.b * 99 / 255 * ARatio),
%    A = floor(Px#px.a * 99 / 255),
%    io:format("~2.. B~2.. B",
%              [B, A]).

letter_to_scanlines(Bin, W) ->
    letter_to_scanlines(Bin, W, []).

%% Only handles binaries with sizes in multiples of W
letter_to_scanlines(<<>>, _, Scanlines) ->
    lists:reverse(Scanlines);
letter_to_scanlines(Bin, W, Scanlines) when size(Bin) >= W ->
    <<Line:W/binary, Rest/binary>> = Bin,
    letter_to_scanlines(Rest, W, [Line | Scanlines]);
letter_to_scanlines(Bin, W, Scanlines) ->
    io:format("Remaining Bin is not long enough for width ~p:~n~p~n",
              [W, Bin]),
    lists:reverse(Scanlines).

%% [[[A], [B]], [[C], [D]]] -> [[A ++ C],[B ++ D]]
combine_scanlines([]) ->
    [];
combine_scanlines([List | Lists]) ->
    Zip = fun(Acc, L) ->
              L ++ Acc
          end,
    Fold = fun(L1, L2) ->
               lists:zipwith(Zip, L1, L2)
           end,
    lists:foldl(Fold, List, Lists).
    %[lists:flatten(L) || L <- lists:foldl(Fold, List, Lists)].

pad_length(Scanline, Length) when length(Scanline) < Length ->
    PaddingLength = Length - length(Scanline),
    Scanline ++ blank_pixels(PaddingLength);
pad_length(Scanline, _) ->
    Scanline.

blank_pixels(N) ->
    BlankPixel = #px{r = 0, g = 0, b = 0, a = 255},
    lists:duplicate(N, BlankPixel).

png(Scanlines, W) ->
    Header = #header{width = W,
                     height = length(Scanlines),
                     bit_depth = 8,
                     color_type = 6,
                     compression = 0,
                     filter = 0,
                     interlace = 0},
    _Png = #png{header = Header,
                background = <<255,0,0>>,
                physical = {W, length(Scanlines), 0},
                srgb = 0, % rendering intent
                text = [],
                data = <<>>,
                pixels = Scanlines,
                other = []}.

%max_height(Letters) ->
    %lists:max([H || {_, {_, _, H, _}, _} <- Letters]).

max_top(Letters) ->
    lists:max([T || {_, {_, _, _, T}, _} <- Letters]).

max_bottom(Letters) ->
    lists:max([H - T || {_, {_, _, H, T}, _} <- Letters]).

max_width(Letters) ->
    lists:max([W || {_, {_, W, _, _}, _} <- Letters]).

longest_scanline(Lists) ->
    SortFun = fun(L1, L2) ->
                      length(L1) < length(L2)
              end,
    [Longest | _] = lists:sort(SortFun, Lists),
    Longest.
