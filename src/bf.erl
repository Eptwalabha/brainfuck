-module(bf).

-export([run/1, run/2]).

% tests
-export([tokenize/1]).
-export([optimize/1]).

run (Code) ->
    run (Code, []).
run (Code, Input) ->
    Tokens = tokenize(Code),
    Optimize = optimize(Tokens),
    Cells = {[], 0, []},
    {_, _, Result} = exec(Optimize, {Cells, Input, []}),
    lists:reverse(Result).

exec (Tokens, Acc) ->
    lists:foldl(fun step/2, Acc, Tokens).

tokenize (Code) ->
    case tokenize_one(Code) of
        eoc -> [];
        {Token, Rest} -> [Token | tokenize(Rest)]
    end.

tokenize_one ([]) -> eoc;
tokenize_one ([$+ | Code]) -> {plus, Code};
tokenize_one ([$- | Code]) -> {minus, Code};
tokenize_one ([$. | Code]) -> {print, Code};
tokenize_one ([$, | Code]) -> {read, Code};
tokenize_one ([$> | Code]) -> {right, Code};
tokenize_one ([$< | Code]) -> {left, Code};
tokenize_one ([$] | _]) -> throw(unexpected_closing_bracket);
tokenize_one ([$[ | Code]) -> tokenize_loop(Code, []);
tokenize_one ([_ | Code]) -> tokenize_one(Code).

tokenize_loop ([$] | Code], Acc) ->
    {{loop, lists:reverse(Acc)}, Code};
tokenize_loop (Code, Acc) ->
    case tokenize_one(Code) of
        eoc -> throw(missing_closing_bracket);
        {Token, Rest} -> tokenize_loop(Rest, [Token | Acc])
    end.

optimize (Tokens) -> optimize (Tokens, []).

optimize (Tokens, [clear, {add, _} | Acc]) ->
    optimize(Tokens, [clear | Acc]);
optimize ([{loop, _} | Tokens], [clear | _] = Acc) ->
    optimize(Tokens, Acc);
optimize ([], Acc) -> lists:reverse(Acc);
optimize ([plus | Tokens], Acc) ->
    case Acc of
        [{add, -1} | Acc2] -> optimize(Tokens, Acc2);
        [{add, N} | Acc2] -> optimize(Tokens, [{add, N + 1} | Acc2]);
        _ -> optimize(Tokens, [{add, 1} | Acc])
    end;
optimize ([minus | Tokens], Acc) ->
    case Acc of
        [{add, 1} | Acc2] -> optimize(Tokens, Acc2);
        [{add, N} | Acc2] -> optimize(Tokens, [{add, N - 1} | Acc2]);
        _ -> optimize(Tokens, [{add, -1} | Acc])
    end;
optimize ([Token | Tokens], Acc)
  when Token =:= left; Token =:= right;
       Token =:= print; Token =:= read ->
    optimize(Tokens, [Token | Acc]);
optimize ([{loop, Loop_tokens} | Tokens], Acc) ->
    case optimize(Loop_tokens, []) of
        [{add, Nbr}] = Tokens2 ->
            case mod(Nbr, 2) of
                1 -> optimize(Tokens, [clear | Acc]);
                _ -> optimize(Tokens, [{loop, Tokens2} | Acc])
            end;
        Tokens2 ->
            case copy_pattern(Tokens2) of
                {copy, _} = Copy -> optimize(Tokens, [Copy | Acc]);
                _ -> optimize(Tokens, [{loop, Tokens2} | Acc])
            end
    end.

copy_pattern ([{add, -1}, right, {add, 1}, left]) -> {copy, 1};
copy_pattern ([right, {add, 1}, left, {add, -1}]) -> {copy, 1};
copy_pattern ([{add, -1}, left, {add, 1}, right]) -> {copy, -1};
copy_pattern ([left, {add, 1}, right, {add, -1}]) -> {copy, -1};
copy_pattern (_) -> [].

step ({copy, 1}, {{Left, Cell, []}, Input, Acc}) ->
    {{Left, 0, [Cell]}, Input, Acc};
step ({copy, 1}, {{Left, Cell, [Cell_r | Right]}, Input, Acc}) ->
    {{Left, 0, [mod(Cell_r + Cell) | Right]}, Input, Acc};
step ({copy, -1}, {{[], Cell, Right}, Input, Acc}) ->
    {{[Cell], 0, Right}, Input, Acc};
step ({copy, -1}, {{[Cell_l | Left], Cell, Right}, Input, Acc}) ->
    {{[mod(Cell_l + Cell) | Left], 0, Right}, Input, Acc};
step (clear, {{Left, _, Right}, Input, Acc}) ->
    {{Left, 0, Right}, Input, Acc};
step ({add, N}, {{Left, Cell, Right}, Input, Acc}) ->
    {{Left, mod(Cell + N), Right}, Input, Acc};
step (print, {{_, Cell, _} = Cells, Input, Acc}) ->
    {Cells, Input, [Cell | Acc]};
step (read, {Cells, [], Acc}) ->
    {Cells, [], Acc};
step (read, {{Left, _, Right}, [Input | Rest], Acc}) ->
    {{Left, Input, Right}, Rest, Acc};
step (right, {{Left, Cell, []}, Input, Acc}) ->
    {{[Cell | Left], 0, []}, Input, Acc};
step (right, {{Left, Cell, [Cell_r | Right]}, Input, Acc}) ->
    {{[Cell | Left], Cell_r, Right}, Input, Acc};
step (left, {{[], Cell, Right}, Input, Acc}) ->
    {{[], 0, [Cell | Right]}, Input, Acc};
step (left, {{[Cell_l | Left], Cell, Right}, Input, Acc}) ->
    {{Left, Cell_l, [Cell | Right]}, Input, Acc};
step ({loop, _}, {{_, 0, _}, _, _} = Acc) -> Acc;
step ({loop, Tokens}, Acc) -> step({loop, Tokens}, exec(Tokens, Acc)).

mod (N) when N > 256 -> mod(N - 256);
mod (N) when N < 0 -> mod(N + 256);
mod (N) -> N.

mod(N, P) -> (((N rem P) + P) rem P).
