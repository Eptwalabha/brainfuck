-module(bf).

-export([run/1, run/2]).

-export([end_of_loop/1]).

run (Code) ->
    run (Code, []).
run (Code, Input) ->
    Cells = {[], 0, []},
    run (Code, Cells, Input, []).

run ([], _, _, Acc) -> lists:reverse(Acc);
run ([$. | Code], {_, Cell, _} = Cells, Input, Acc) ->
    run(Code, Cells, Input, [Cell | Acc]);
run ([$, | Code], {Left, _, Right}, [], Acc) ->
    run(Code, {Left, 0, Right}, [], Acc);
run ([$, | Code], {Left, _, Right}, [Input | Rest], Acc) ->
    run(Code, {Left, Input, Right}, Rest, Acc);
run ([$+ | Code], {Left, Cell, Right}, Input, Acc) ->
    run(Code, {Left, (Cell + 1) rem 256, Right}, Input, Acc);
run ([$- | Code], {Left, Cell, Right}, Input, Acc) ->
    run(Code, {Left, (Cell + 255) rem 256, Right}, Input, Acc);
run ([$> | Code], {[], Cell, Right}, Input, Acc) ->
    run(Code, {[], 0, [Cell | Right]}, Input, Acc);
run ([$> | Code], {[Cell_l | Left], Cell, Right}, Input, Acc) ->
    run(Code, {Left, Cell_l, [Cell | Right]}, Input, Acc);
run ([$< | Code], {Left, Cell, []}, Input, Acc) ->
    run(Code, {[Cell | Left], 0, []}, Input, Acc);
run ([$< | Code], {Left, Cell, [Cell_r | Right]}, Input, Acc) ->
    run(Code, {[Cell | Left], Cell_r, Right}, Input, Acc);
run ([$[ | Code], {_, 0, _} = Cells, Input, Acc) ->
    run(end_of_loop(Code), Cells, Input, Acc);
run ([$[ | Code], Cells, Input, Acc) ->
    case run(Code, Cells, Input, Acc) of
        {Code2, {_, 0, _} = Cells2, Input2, Acc2} ->
            run(Code2, Cells2, Input2, Acc2);
        {_, Cells2, Input2, Acc2} ->
            run([$[ | Code], Cells2, Input2, Acc2)
    end;
run ([$] | Code], Cells, Input, Acc) ->
    {Code, Cells, Input, Acc};
run ([_ | Code], Cells, Input, Acc) ->
    run(Code, Cells, Input, Acc).

end_of_loop (Code) -> end_of_loop(Code, 1).

end_of_loop ([], _) -> throw(no_end_bracket);
end_of_loop ([$] | Code], 1) -> Code;
end_of_loop ([$] | Code], Nbr) -> end_of_loop(Code, Nbr - 1);
end_of_loop ([$[ | Code], Nbr) -> end_of_loop(Code, Nbr + 1);
end_of_loop ([_ | Code], Nbr) -> end_of_loop(Code, Nbr).
