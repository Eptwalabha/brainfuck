-module(bf_test).

-include_lib("eunit/include/eunit.hrl").

optimize (Tokens) -> bf:optimize(Tokens).
tokenize (Code) -> bf:tokenize(Code).
optimize_code (Code) -> optimize(tokenize(Code)).

print_test_ () ->
    [return_an_empty_list_if_nothing_is_printed(),
     print_cell_s_value()].

input_test_ () ->
    [put_input_s_value_into_current_cell(),
     leave_current_value_if_no_input_available()].

opperation_on_cells_test_ () ->
    [increase_cell(),
     decrease_cell(),
     wrap_cell_value_from_0_to_255()].

shifting_cell_s_cursor_test_ () ->
    [a_new_cell_is_set_to_zero(),
     shift_the_cursor_to_the_right(),
     shift_the_cursor_to_the_left(),
     shift_the_cursor_wherever_you_want()].

loop_test_ () ->
    [jump_to_end_of_loop_if_entering_it_when_current_cell_is_zero(),
     exit_the_loop_when_current_cell_s_value_is_zero(),
     loop_consumes_input()].

misc_test_ () ->
    [does_not_interpret_non_valid_symboles(),
     hello_world(),
     quick_sort()].

lexing_test_ () ->
    [tokenize_symboles(),
     tokenize_only_valid_symboles(),
     tokenize_loop(),
     tokenize_nested_loop(),
     throw_exception_on_unexpected_closing_bracket(),
     throw_exception_on_missing_closing_bracket()].

optimize_test_ () ->
    [contraction_plus_minus(),
     contraction_with_other_symboles(),
     contraction_with_loop(),
     clear_loop(),
     potential_and_infinit_loop(),
     copy_loop()].

clean_code_test_ () ->
    [remove_unreachable_loop(),
     simplify_add_then_clear()].

return_an_empty_list_if_nothing_is_printed () ->
    ?_assertEqual([], bf:run("++><")).

print_cell_s_value () ->
    [?_assertEqual([0], bf:run(".")),
     ?_assertEqual([0, 0], bf:run("..")),
     ?_assertEqual([0, 1, 2, 2], bf:run(".+.+.."))].

put_input_s_value_into_current_cell () ->
    ?_assertEqual([0, 5], bf:run(".,.", [5])).

leave_current_value_if_no_input_available () ->
    [?_assertEqual([0], bf:run(",.")),
     ?_assertEqual([42, 42], bf:run(",.,.", [42])),
     ?_assertEqual([1, 5, 0], bf:run(",.>,.>,.", [1, 5])),
     ?_assertEqual([1, 1, 1], bf:run(",.,.,.", [1]))].

increase_cell () ->
    [?_assertEqual([1], bf:run("+.")),
     ?_assertEqual([1], bf:run("+.++")),
     ?_assertEqual([5], bf:run("+++++."))].

decrease_cell () ->
    [?_assertEqual([0], bf:run("+-.")),
     ?_assertEqual([1], bf:run("++-."))].

wrap_cell_value_from_0_to_255 () ->
    [?_assertEqual([(5 - 1)], bf:run(",-.", [5])),
     ?_assertEqual([(256 - 5)], bf:run("-----.")),
     ?_assertEqual([254, 1], bf:run("--.+++."))].

a_new_cell_is_set_to_zero () ->
    ?_assertEqual([2, 0], bf:run("++.>.")).

shift_the_cursor_to_the_right () ->
    ?_assertEqual([0, 0, 0], bf:run(".++>.++>.")).

shift_the_cursor_to_the_left () ->
    ?_assertEqual([0, 0, 0], bf:run(".<.++<.")).

shift_the_cursor_wherever_you_want () ->
    [?_assertEqual([0, 2, 4], bf:run(".++>++++<.>.")),
     ?_assertEqual([0, 0, 3], bf:run(".+++>>>.<<<.")),
     ?_assertEqual([0, 0, 0, 1, 2, 3], bf:run(".+>.++>.+++<<.>.>."))].

jump_to_end_of_loop_if_entering_it_when_current_cell_is_zero () ->
    [?_assertEqual([0, 0], bf:run(".[++++>].")),
     ?_assertEqual([], bf:run("[....]"))].

exit_the_loop_when_current_cell_s_value_is_zero () ->
    [?_assertEqual([1, 4], bf:run("+.[+++>]<.")),
     ?_assertEqual([3, 0, 6], bf:run("+++.[>++<-].>."))].

loop_consumes_input () ->
    [?_assertEqual([6], bf:run(",[>,<-]>.", [2, 1, 6, 8])),
     ?_assertEqual([3], bf:run("++[>,<-]>.", [2, 3])),
     ?_assertEqual([3], bf:run("+++++[>,<-]>.", [2, 3])),
     ?_assertEqual([3, 2, 1, 0, 0], bf:run(",.[,.].", [3, 2, 1, 0]))].

does_not_interpret_non_valid_symboles () ->
    ?_assertEqual([], bf:run("abcdefg")).

hello_world () ->
    Program = hello_world_program(),
    ?_assertEqual("Hello World", bf:run(Program)).

quick_sort () ->
    Quicksort = quick_sort_program(),
    [?_assertEqual("abcdefg", bf:run(Quicksort, "gfedcba")),
     ?_assertEqual("aaddeew", bf:run(Quicksort, "wdeaaed"))].

hello_world_program () ->
    "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.++++"
    "+++..+++.>++.<<+++++++++++++++.>.+++.------.--------.".

quick_sort_program () ->
    ">>,[>>,]<<[[<<]>>>>[<<[>+<<+>-]>>[>+<<<<[->]>[<]>>-]<<<[[-]"
    ">>[>+<-]>>[<<<+>>>-]]>>[[<+>-]>>]<]<<[>>+<<-]<<]>>>>[.>>]".

tokenize_symboles () ->
    [?_assertEqual([plus], tokenize("+")),
     ?_assertEqual([minus], tokenize("-")),
     ?_assertEqual([print], tokenize(".")),
     ?_assertEqual([read], tokenize(",")),
     ?_assertEqual([left], tokenize("<")),
     ?_assertEqual([right], tokenize(">")),
     ?_assertEqual([plus, minus, left, read, right, print],
                   tokenize("+-<,>."))].

tokenize_only_valid_symboles () ->
    Code = "a+b-c.d,efg>h<i",
    Expected = [plus, minus, print, read, right, left],
    ?_assertEqual(Expected, tokenize(Code)).

tokenize_loop () ->
    Code = "+[->+abcdefg+<]>.",
    Expected = [plus, {loop, [minus, right, plus, plus, left]},
                right, print],
    [?_assertEqual([{loop, [left]}], tokenize("[<]")),
     ?_assertEqual(Expected, tokenize(Code))].

tokenize_nested_loop () ->
    Expected = [plus,
                {loop,
                 [{loop, [right]},
                  right,
                  {loop,
                   [{loop, [plus, right]},
                    plus, plus, right]}]},
                plus],
    ?_assertEqual(Expected, tokenize("+[[>]>[[+>]++>]]+")).

throw_exception_on_unexpected_closing_bracket () ->
    [?_assertThrow(unexpected_closing_bracket, tokenize("+-+]")),
     ?_assertThrow(unexpected_closing_bracket, tokenize("+[>++<-]-+]"))].

throw_exception_on_missing_closing_bracket () ->
    ?_assertThrow(missing_closing_bracket, tokenize("+[-+>]++[--")).

contraction_plus_minus () ->
    [?_assertEqual([{add, 4}], optimize([plus, plus, plus, plus])),
     ?_assertEqual([{add, -3}], optimize([minus, minus, minus])),
     ?_assertEqual([{add, 1}], optimize([minus, plus, plus])),
     ?_assertEqual([], optimize([minus, plus, minus, plus])),
     ?_assertEqual([{add, 42}], optimize(lists:duplicate(42, plus)))].

contraction_with_other_symboles () ->
    [?_assertEqual([{add, 1}, left, {add, -1}, right],
                   optimize([plus, left, minus, right])),
     ?_assertEqual([left, left],
                   optimize([plus, minus, left, minus, plus, left])),
     ?_assertEqual([left, {add, 2}, print, read],
                   optimize([plus, minus, left, plus, plus, print, read]))].

contraction_with_loop () ->
    [?_assertEqual([{add, 2}, {loop, [{add, -1}, print]}],
                   optimize_code("++[-.]")),
     ?_assertEqual([{loop, [{loop, [{add, 2}, left]}]}, left, print],
                   optimize_code("+-[+-[++<]+-+-]<+-."))].

clear_loop () ->
    [?_assertEqual([clear], optimize_code("[+]")),
     ?_assertEqual([clear], optimize_code("[+-+]")),
     ?_assertEqual([clear], optimize_code("[-]")),
     ?_assertEqual([clear], optimize_code("[+++]")),
     ?_assertEqual([clear], optimize_code("[---]"))].

potential_and_infinit_loop () ->
    [?_assertEqual([{loop, [{add, 2}]}], optimize_code("[++]")),
     ?_assertEqual([{loop, [{add, -4}]}], optimize_code("[----]")),
     ?_assertEqual([{loop, []}], optimize_code("[]"))].

remove_unreachable_loop () ->
    [?_assertEqual([read, clear, print], optimize_code(",[-][->++<].")),
     ?_assertEqual([read, clear, print], optimize_code(",[-][+][-]."))].

copy_loop () ->
    [?_assertEqual([read, {copy, 1}], optimize_code(",[->+<]")),
     ?_assertEqual([read, {copy, 1}], optimize_code(",[>+<-]")),
     ?_assertEqual([2, 7], bf:run(",>,.<[->+<]>.", [5, 2])),
     ?_assertEqual([0, 42], bf:run(",[>+<-].>.", [42])),
     ?_assertEqual([read, {copy, -1}], optimize_code(",[<+>-]")),
     ?_assertEqual([read, {copy, -1}], optimize_code(",[-<+>]")),
     ?_assertEqual([0, 6], bf:run("<,>,[-<+>].<.", [5, 1])),
     ?_assertEqual([0, 42], bf:run(",[<+>-].<.", [42]))].

simplify_add_then_clear () ->
    [?_assertEqual([read, clear], optimize_code(",++[-]")),
     ?_assertEqual([read, clear], optimize_code(",--[+]"))].
