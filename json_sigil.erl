-module(json_sigil).
-export([parse_transform/2]).

parse_transform(Forms0, _Options) ->
    Forms = [erl_syntax_lib:map(fun(Node) ->
                 transform_node(erl_syntax:revert(Node))
             end, F) || F <- Forms0],
    erl_syntax:revert_forms(Forms).

transform_node({sigil, _Anno, json, String, _Suffix}) ->
    Bin = erl_syntax:binary([erl_syntax:binary_field(
                             String,
                             [erl_syntax:atom(utf8)])]),
    erl_syntax:application(erl_syntax:atom(json),
                           erl_syntax:atom(decode),
                           [Bin]);
transform_node(Node) ->
    Node.
