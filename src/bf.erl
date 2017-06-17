-module(bf).

-export([run/1, run/2]).
-export([parse/1]).

run (Code) ->
    run (Code, []).
run (Code, Input) ->
    Tokens = parse(Code),
    Cells = {[], 0, []},
    {_, _, Result} = exec(Tokens, {Cells, Input, []}),
    lists:reverse(Result).

exec (Tokens, Acc) ->
    lists:foldl(fun step/2, Acc, Tokens).

parse (Code) ->
    case parse_one(Code) of
        eoc -> [];
        {Token, Rest} -> [Token | parse(Rest)]
    end.

parse_one ([]) -> eoc;
parse_one ([$+ | Code]) -> {plus, Code};
parse_one ([$- | Code]) -> {minus, Code};
parse_one ([$. | Code]) -> {print, Code};
parse_one ([$, | Code]) -> {read, Code};
parse_one ([$> | Code]) -> {right, Code};
parse_one ([$< | Code]) -> {left, Code};
parse_one ([$] | _]) -> throw(unexpected_end_of_loop);
parse_one ([$[ | Code]) -> parse_loop(Code, []);
parse_one ([_ | Code]) -> parse_one(Code).

parse_loop ([$] | Code], Acc) ->
    {{loop, lists:reverse(Acc)}, Code};
parse_loop (Code, Acc) ->
    case parse_one(Code) of
        eoc -> {{loop, lists:reverse(Acc)}, []};
        {Token, Rest} -> parse_loop(Rest, [Token | Acc])
    end.

step (plus, {{Left, Cell, Right}, Input, Acc}) ->
    {{Left, (Cell + 1) rem 256, Right}, Input, Acc};
step (minus, {{Left, Cell, Right}, Input, Acc}) ->
    {{Left, (Cell + 255) rem 256, Right}, Input, Acc};
step (print, {{_, Cell, _} = Cells, Input, Acc}) ->
    {Cells, Input, [Cell | Acc]};
step (read, {{Left, _, Right}, [], Acc}) ->
    {{Left, 0, Right}, [], Acc};
step (read, {{Left, _, Right}, [Input | Rest], Acc}) ->
    {{Left, Input, Right}, Rest, Acc};
step (left, {{Left, Cell, []}, Input, Acc}) ->
    {{[Cell | Left], 0, []}, Input, Acc};
step (left, {{Left, Cell, [Cell_r | Right]}, Input, Acc}) ->
    {{[Cell | Left], Cell_r, Right}, Input, Acc};
step (right, {{[], Cell, Right}, Input, Acc}) ->
    {{[], 0, [Cell | Right]}, Input, Acc};
step (right, {{[Cell_l | Left], Cell, Right}, Input, Acc}) ->
    {{Left, Cell_l, [Cell | Right]}, Input, Acc};
step ({loop, _}, {{_, 0, _}, _, _} = Acc) -> Acc;
step ({loop, Tokens}, Acc) -> step({loop, Tokens}, exec(Tokens, Acc)).
