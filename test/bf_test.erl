-module(bf_test).

-include_lib("eunit/include/eunit.hrl").

print_test_ () ->
    [
     return_an_empty_list_if_nothing_is_printed(),
     print_cell_s_value()
    ].

input_test_ () ->
    [
     put_input_s_value_into_current_cell(),
     set_current_cell_to_zero_if_there_is_no_input()
    ].

opperation_on_cells_test_ () ->
    [
     increase_cell(),
     decrease_cell(),
     wrap_cell_value_from_0_to_255()
    ].

shifting_cell_s_cursor_test_ () ->
    [
     a_new_cell_is_set_to_zero(),
     shift_the_cursor_to_the_right(),
     shift_the_cursor_to_the_left(),
     shift_the_cursor_wherever_you_want()
    ].

loop_test_ () ->
    [
     jump_to_end_of_loop_if_entering_it_when_current_cell_is_zero(),
     exit_the_loop_when_current_cell_s_value_is_zero(),
     loop_consumes_input()
    ].

end_of_loop_test_ () ->
    [
     return_rest_code_after_closing_bracket(),
     take_nested_loops_into_account(),
     throw_an_error_if_dont_find_end_bracket()
    ].

misc_test_ () ->
    [
     does_not_interpret_non_valid_symboles(),
     hello_world(),
     quick_sort()
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

return_rest_code_after_closing_bracket () ->
    ?_assertEqual("after", bf:end_of_loop("before]after")).

take_nested_loops_into_account () ->
    [?_assertEqual("the rest", bf:end_of_loop("a[b]c]the rest")),
     ?_assertEqual("a[toto]b", bf:end_of_loop("tata]a[toto]b"))].

throw_an_error_if_dont_find_end_bracket () ->
    [?_assertThrow(no_end_bracket, bf:end_of_loop("abcd")) ].

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
