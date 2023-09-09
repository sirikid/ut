-module(ut_tests).

-include_lib("eunit/include/eunit.hrl").

expand_test_() ->
    [Test || File <- ["ref-tests/extended-tests.json",
                      "ref-tests/negative-tests.json",
                      "ref-tests/spec-examples-by-section.json",
                      "ref-tests/spec-examples.json"],
             Test <- tests_from_file(File)].

tests_from_file(File) ->
    {ok, Json} = file:read_file(File),
    {ok, Groups} = thoas:decode(Json),
    lists:flatmap(
      fun({Title, #{<<"variables">> := Variables, <<"testcases">> := TestCases}})->
              lists:map(
                fun([Template, ExpectedResult]) ->
                        Name = string:join([File, binary_to_list(Title), binary_to_list(Template)], ": "),
                        Test =
                            case ExpectedResult of
                                false ->
                                    ?_assertException(_, _, ut:expand(Template, Variables));
                                <<Expansion/binary>> ->
                                    ?_assertEqual(Expansion, ut:expand(Template, Variables));
                                Expansions when is_list(Expansions) ->
                                    ?_assert(lists:member(ut:expand(Template, Variables), Expansions))
                            end,
                        {Name, Test}
                end,
                TestCases)
      end,
      maps:to_list(Groups)).
