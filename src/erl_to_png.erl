-module(erl_to_png).

-include("png.hrl").

-export([render/1]).
-export([render/2]).

% Test exports
-export([filter_tuples/1]).
-export([render_tuples/1]).
-export([render_tuples/2]).
-export([lines/1]).
-export([lines/2]).
-export([scanlines/3]).
-export([pad_length/2]).
-export([max_height/1]).
-export([max_top/1]).
-export([longest_scanline/1]).
-export([combine_scanlines/1]).
-export([blank_pixels/1]).

render(Filename) ->
    render(Filename, []).

render(Filename, IncludePaths) ->
    Tuples0 = erl_to_tuples:get_tuples(Filename, IncludePaths),
    Tuples = lists:filter(fun filter_tuples/1, Tuples0),
    Letters = render_tuples(Tuples),
    MaxHeight = max_height(Letters),
    MaxTop = max_top(Letters),
    MaxBottom = MaxHeight - MaxTop,
    Lines = lines(Letters),
    Scanlines0  = scanlines(Lines, MaxTop, MaxBottom),
    Longest = longest_scanline(Scanlines0),
    MaxLength = length(Longest),
    Scanlines = [pad_length(SL, MaxLength) || SL <- Scanlines0],
    Png = png(Scanlines),
    erl_png:write_scanlines(Png, Filename ++ ".png").

filter_tuples(Tuple = {_, <<>>, _}) ->
    io:format("Filter out tuple with empty bin: ~p~n", [Tuple]),
    false;
filter_tuples(Tuple = {_, Bin, _}) when is_binary(Bin) and size(Bin) == 0 ->
    io:format("Filter out tuple with size zero bin: ~p~n", [Tuple]),
    false;
filter_tuples(NotTuple) when not(is_tuple(NotTuple)) ->
    io:format("Filter out non-tuple: ~p~n", [NotTuple]),
    false;
filter_tuples({_, NotBin, _}) when not(is_binary(NotBin)) ->
    io:format("Filter out non-binary: ~p~n", [NotBin]),
    false;
filter_tuples(_) ->
    true.

render_tuples(Tuples) ->
    render_tuples(Tuples, []).

render_tuples([], Letters) ->
    lists:reverse(Letters);
render_tuples([{_, <<>>, _} | Tuples], Letters) ->
    render_tuples(Tuples, Letters);
render_tuples([{Line, <<Char:1/binary, Bin/binary>>, Colour} | Tuples], Letters) ->
    Letter = render_char:render_char(binary_to_list(Char)),
    render_tuples([{Line, Bin, Colour} | Tuples],
                  [{Line, Letter, Colour} | Letters]);
render_tuples([{Line, <<Char:1/binary>>, Colour} | Tuples], Letters) ->
    Letter = render_char:render_char(binary_to_list(Char)),
    render_tuples(Tuples,
                  [{Line, Letter, Colour}| Letters]);
render_tuples([BadTuple = {_, Bin, _} | Tuples], Letters) ->
    io:format("Skipping bad tuple: ~p with bin size ~p~n", [BadTuple, size(Bin)]),
    render_tuples(Tuples, Letters).


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
% Replace the out-of-place line number with the current
% line number since it isn't an int and greater than the
% current line number
lines([{_, X, Y} | Letters],
      [Line = [{LineNo, _, _} | _] | Lines]) ->
    lines(Letters, [[{LineNo, X, Y} | Line] | Lines]).

scanlines(Lines0, MaxT, MaxB) ->
    % for each line
    %    for each letter
    %        generate scanlines
    %    combine letters
    % find max width
    % compact all lines into single binaries
    % DONE - these get passed to erl_png
    Lines = lists:flatten(lines_to_scanlines(Lines0, MaxT, MaxB)),
    MaxLength = lists:max(lists:map(fun length/1, Lines)),
    % increase all shorter lines
    _EqualLines = [pad_length(L, MaxLength) || L <- Lines].

lines_to_scanlines(Lines, MaxT, MaxB) ->
    lines_to_scanlines(Lines, MaxT, MaxB, []).

lines_to_scanlines([], _, _, Scanlines) ->
    lists:reverse(Scanlines);
lines_to_scanlines([Line | Lines], MaxT, MaxB, Scanlines) ->
    Letters = letters_to_scanlines(Line, MaxT, MaxB),
    Scanline = combine_scanlines(Letters),
    lines_to_scanlines(Lines, MaxT, MaxB, [Scanline | Scanlines]).

letters_to_scanlines(Letters, MaxT, MaxB) ->
    letters_to_scanlines(Letters, MaxT, MaxB, []).

% {Line, {Bin, W, H, T}, Colour}
letters_to_scanlines([Letter | Letters], MaxT, MaxB, Scanlines) ->
    {_, {Bin, W, H, T}, Colour} = Letter,
    Scanlines0 = letter_to_scanlines(Bin, W),
    Pixels = pixels(Scanlines0, Colour),
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

letter_to_scanlines(Bin, W) ->
    letter_to_scanlines(Bin, W, []).

%% Only handles binaries with sizes in multiples of W
letter_to_scanlines(<<>>, _, Scanlines) ->
    lists:reverse(Scanlines);
letter_to_scanlines(Bin, W, Scanlines) ->
    <<Line:W/binary, Rest/binary>> = Bin,
    letter_to_scanlines(Rest, W, [Line | Scanlines]).

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

pad_length(Scanline, Length) when length(Scanline) < Length ->
    PaddingLength = Length - length(Scanline),
    Scanline ++ blank_pixels(PaddingLength);
pad_length(Scanline, _) ->
    Scanline.

blank_pixels(N) ->
    BlankPixel = #px{r = 0, g = 0, b = 0, a = 255},
    lists:duplicate(N, BlankPixel).

png(Scanlines) ->
    Header = #header{width = 400,
                     height = 40,
                     bit_depth = 8,
                     color_type = 6,
                     compression = 0,
                     filter = 0,
                     interlace = 0},
    _Png = #png{header = Header,
                background = <<255,0,0>>,
                physical = {400, 40, 0},
                srgb = 0, % rendering intent
                text = [],
                data = <<>>,
                pixels = Scanlines,
                other = []}.

max_height(Letters) ->
    lists:max([H || {_, {_, _, H, _}, _} <- Letters]).

max_top(Letters) ->
    lists:max([T || {_, {_, _, _, T}, _} <- Letters]).

longest_scanline(Lists) ->
    SortFun = fun(L1, L2) ->
                      length(L1) < length(L2)
              end,
    [Longest | _] = lists:sort(SortFun, Lists),
    Longest.
