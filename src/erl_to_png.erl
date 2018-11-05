-module(erl_to_png).

-include("png.hrl").

-export([render/1]).
-export([render/2]).

% Test exports
-export([filter_tuples/1]).
-export([render_tuples/1]).
-export([render_tuples/2]).
-export([lines/2]).
-export([scanlines/3]).
-export([scanlines/4]).
-export([pad_length/2]).
-export([max_height/1]).
-export([max_top/1]).
-export([longest_scanline/1]).

render(Filename) ->
    render(Filename, []).

render(Filename, IncludePaths) ->
    Tuples0 = erl_to_tuples:get_tuples(Filename, IncludePaths),
    Tuples = lists:filter(fun filter_tuples/1, Tuples0),
    Letters = render_tuples(Tuples),
    MaxHeight = max_height(Letters),
    MaxTop = max_top(Letters),
    Lines = lines(Letters),
    Scanlines0  = scanlines(Lines, MaxHeight, MaxTop),
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
    render_tuples([], Tuples).

render_tuples(Letters, []) ->
    lists:reverse(Letters);
render_tuples(Letters, [{_, <<>>, _} | Tuples]) ->
    render_tuples(Letters, Tuples);
render_tuples(Letters, [{Line, <<Char:1/binary, Bin/binary>>, Colour} | Tuples]) ->
    Letter = render_char:render_char(binary_to_list(Char)),
    render_tuples([Letter | Letters], [{Line, Bin, Colour} | Tuples]);
render_tuples(Letters, [{Line, <<Char:1/binary>>, Colour} | Tuples]) ->
    Letter = render_char:render_char(binary_to_list(Char)),
    render_tuples([{Line, Letter, Colour}| Letters], Tuples);
render_tuples(Letters, [BadTuple = {_, Bin, _} | Tuples]) ->
    io:format("Skipping bad tuple: ~p with bin size ~p~n", [BadTuple, size(Bin)]),
    render_tuples(Letters, Tuples).


lines(Letters) ->
    lines(Letters, [[]]).

lines([], Lines) ->
    lists:reverse([lists:reverse(L) || L <- Lines]);
lines([Letter | Letters], [[] | Lines]) ->
    lines(Letters, [[Letter] | Lines]);
% Ignore line numbers that are out of order and lines
% that are 'noline'
lines([Letter = {Line, _, _} | Letters],
      Lines = [[{DiffLine, _, _} | Letters] | Lines])
    when is_integer(Line),
         is_integer(DiffLine),
         Line > DiffLine ->
    lines(Letters, [[Letter] | Lines]);
lines([Letter | Letters], [Line | Lines]) ->
    lines(Letters, [[Letter | Line] | Lines]).

scanlines(Lines, MaxH, MaxT) when is_list(Lines) ->
    Scanlines = fun(Line) ->
                        lists:reverse(
                          scanlines(Line,
                                    MaxH,
                                    MaxT))
                end,
    lists:flatten([Scanlines(Line) || Line <- Lines]);
scanlines(Line, Height, Top) ->
    Scanlines = << <<0>> || _ <- lists:seq(1, Height)>>,
    scanlines(Line, Height, Top, Scanlines).

scanlines([], _, _, Scanlines) ->
    Scanlines;
scanlines([Letter | Line], MaxH, MaxT, Scanlines0) ->
    Scanlines = scanlines(Letter, MaxH, MaxT, Scanlines0, 1),
    scanlines(Line, MaxH, MaxT, Scanlines).

% We have MaxHeight (i.e. all) scanlines
scanlines(_, MaxH, _, Scanlines, LineNo) when MaxH == LineNo ->
    Scanlines;
% We have all the scanlines
scanlines({_, _, H, _}, MaxH, _, Scanlines, _)
    when H == MaxH ->
    Scanlines;
% This top is lower than the max top, so we'll pad the top with a blank
% line
scanlines(Letter = {_, W, _, T}, MaxH, MaxT, Scanlines, LineNo)
    when T < MaxT ->
    BlankLine = list_to_binary([0 || _ <- lists:seq(1, W)]),
    scanlines(Letter, MaxH, MaxT, [BlankLine | Scanlines], LineNo + 1);
% The area below the baseline needs to be padded
scanlines(Letter = {_, W, H, T}, MaxH, MaxT, Scanlines, LineNo)
    when (MaxT - T) + H < LineNo ->
    BlankLine = list_to_binary([0 || _ <- lists:seq(1, W)]),
    scanlines(Letter, MaxH, MaxT, [BlankLine | Scanlines], LineNo + 1);
% We're between Top and (Height - Top), which means there's data
scanlines(Letter = {Bin, W, _, T}, MaxH, MaxT, Scanlines, LineNo) ->
    TopPadding = MaxT - T,
    Line = LineNo - TopPadding,
    case Line of
        0 ->
            <<Scanline:W/binary, _/binary>> = Bin;
        X ->
            Skip = X * W,
            <<_:Skip/binary, Scanline:W/binary, _/binary>> = Bin
    end,
    scanlines(Letter, MaxH, MaxT, [Scanline | Scanlines], LineNo + 1).

pad_length(Scanline, MaxLine) when size(Scanline) < MaxLine ->
    PaddingLength = MaxLine - size(Scanline),
    Padding = list_to_binary([0 || _ <- lists:seq(1, PaddingLength)]),
    <<Scanline/binary, Padding/binary>>;
pad_length(Scanline, MaxLine) ->
    <<Scanline:MaxLine/binary>>.

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
    lists:max([H || {_, _, H, _} <- Letters]).

max_top(Letters) ->
    lists:max([T || {_, _, _, T} <- Letters]).

longest_scanline(Lists) ->
    SortFun = fun(L1, L2) ->
                      length(L1) < length(L2)
              end,
    [Longest | _] = lists:sort(SortFun, Lists),
    Longest.
