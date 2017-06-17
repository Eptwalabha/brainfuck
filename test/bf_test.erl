-module(bf_test).

-include_lib("eunit/include/eunit.hrl").

print_test_ () ->
    [return_an_empty_list_if_nothing_is_printed(),
     print_cell_s_value()].

input_test_ () ->
    [put_input_s_value_into_current_cell(),
     set_current_cell_to_zero_if_there_is_no_input()].

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

parsing_test_ () ->
    [parse_symboles(),
     parse_only_valid_symboles(),
     parse_loop(),
     parse_nested_loop(),
     throw_exception_on_unexpected_closing_bracket(),
     throw_exception_on_missing_closing_bracket()
    ].

return_an_empty_list_if_nothing_is_printed () ->
    ?_assertEqual([], bf:run("++><")).

print_cell_s_value () ->
    [?_assertEqual([0], bf:run(".")),
     ?_assertEqual([0, 0], bf:run("..")),
     ?_assertEqual([0, 1, 2, 2], bf:run(".+.+.."))].

put_input_s_value_into_current_cell () ->
    ?_assertEqual([0, 5], bf:run(".,.", [5])).

set_current_cell_to_zero_if_there_is_no_input () ->
    [?_assertEqual([0], bf:run(",.")),
     ?_assertEqual([1, 5, 0], bf:run(",.>,.>,.", [1, 5])),
     ?_assertEqual([1, 0, 0], bf:run(",.,.,.", [1]))].

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
     ?_assertEqual([3, 2, 1, 0, 0], bf:run(",.[,.].", [3, 2, 1]))].

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

parse_symboles () ->
    [?_assertEqual([plus], bf:parse("+")),
     ?_assertEqual([minus], bf:parse("-")),
     ?_assertEqual([print], bf:parse(".")),
     ?_assertEqual([read], bf:parse(",")),
     ?_assertEqual([left], bf:parse("<")),
     ?_assertEqual([right], bf:parse(">")),
     ?_assertEqual([plus, minus, left, read, right, print],
                   bf:parse("+-<,>."))].

parse_only_valid_symboles () ->
    Code = "a+b-c.d,efg>h<i",
    Expected = [plus, minus, print, read, right, left],
    ?_assertEqual(Expected, bf:parse(Code)).

parse_loop () ->
    Code = "+[->+abcdefg+<]>.",
    Expected = [plus, {loop, [minus, right, plus, plus, left]},
                right, print],
    [?_assertEqual([{loop, [left]}], bf:parse("[<]")),
     ?_assertEqual(Expected, bf:parse(Code))].

parse_nested_loop () ->
    Code = "+[[>]>[[+>]++>]]+",
    Expected = [plus,
                {loop,
                 [{loop, [right]},
                  right,
                  {loop,
                   [{loop, [plus, right]},
                    plus, plus, right]}]},
                plus],
    ?_assertEqual(Expected, bf:parse(Code)).

throw_exception_on_unexpected_closing_bracket () ->
    [?_assertThrow(unexpected_closing_bracket, bf:parse("+-+]")),
     ?_assertThrow(unexpected_closing_bracket, bf:parse("+[>++<-]-+]"))].

throw_exception_on_missing_closing_bracket () ->
    ?_assertThrow(missing_closing_bracket, bf:parse("+[-+>]++[--")).
