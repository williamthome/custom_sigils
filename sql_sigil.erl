-module(sql_sigil).
-export([parse_transform/2]).

parse_transform(Forms0, _Options) ->
    Forms = [erl_syntax_lib:map(fun(Node) ->
                 transform_node(erl_syntax:revert(Node))
             end, F) || F <- Forms0],
    erl_syntax:revert_forms(Forms).

transform_node({sigil, Anno, sql, String, _Suffix}) ->
    build_str(Anno, String);
transform_node(Node) ->
    Node.

build_str(Anno, String) ->
    Str = erl_syntax:string_value(String),
    Elems = elems(Anno, Str),
    {SqlElems, Params} = build_query(Elems, [], [], 1),
    Sql = {bin, Anno, SqlElems},
    erl_syntax:revert(erl_syntax:tuple([erl_syntax:atom(sql),
                                        Sql,
                                        erl_syntax:list(Params)])).

build_query([], BAcc, PAcc, _PIdx) ->
    {lists:reverse(BAcc), lists:reverse(PAcc)};
build_query([{string, Anno, Str} | T], BAcc, PAcc, PIdx) ->
    BElem = {bin_element, Anno, {string, Anno, Str}, default, [utf8]},
    build_query(T, [BElem | BAcc], PAcc, PIdx);
build_query([{block, Anno, Block} | T], BAcc, PAcc, PIdx) ->
    BElem = {bin_element, Anno, {string, Anno, "$" ++ integer_to_list(PIdx)}, default, [utf8]},
    build_query(T, [BElem | BAcc], [Block | PAcc], PIdx + 1).

elems(Anno, Str) ->
    Ln = erl_anno:line(Anno),
    elems(Str, Ln, Ln, []).

elems([], _StrLn, _ExprLn, []) ->
    [];
elems([], StrLn, _ExprLn, Acc) ->
    [str_tuple(StrLn, Acc)];
elems([$\\, ${ | T], StrLn, ExprLn, Acc) ->
    elems(T, StrLn, ExprLn, [${ | Acc]);
elems([${ | T0], _StrLn, ExprLn, []) ->
    {ExprElem, T, EndLn} = expr_elem(T0, 1, ExprLn, ExprLn, []),
    [ExprElem | elems(T, EndLn, EndLn, [])];
elems([${ | T0], StrLn, ExprLn, Acc) ->
    StrElem = str_tuple(StrLn, Acc),
    {ExprElem, T, EndLn} = expr_elem(T0, 1, ExprLn, ExprLn, []),
    [StrElem, ExprElem | elems(T, EndLn, EndLn, [])];
elems([$\r, $\n | T], StrLn, ExprLn, Acc) ->
    elems(T, StrLn, ExprLn + 1, [$\n, $\r | Acc]);
elems([$\r | T], StrLn, ExprLn, Acc) ->
    elems(T, StrLn, ExprLn + 1, [$\r | Acc]);
elems([$\n | T], StrLn, ExprLn, Acc) ->
    elems(T, StrLn, ExprLn + 1, [$\n | Acc]);
elems([H | T], StrLn, ExprLn, Acc) ->
    elems(T, StrLn, ExprLn, [H | Acc]).

expr_elem([$} | T], Depth, ExprLn, StrLn, Acc) ->
    case Depth - 1 of
        0 ->
            Elem = block_tuple(ExprLn, Acc),
            case T of
                [$\r, $\n | _] ->
                    {Elem, T, StrLn + 1};
                [$\r | _] ->
                    {Elem, T, StrLn + 1};
                [$\n | _] ->
                    {Elem, T, StrLn + 1};
                _ ->
                    {Elem, T, StrLn}
            end;
        _ ->
            expr_elem(T, Depth - 1, ExprLn, StrLn, [$} | Acc])
    end;
expr_elem([${ | T], Depth, ExprLn, StrLn, Acc) ->
    expr_elem(T, Depth + 1, ExprLn, StrLn, [${ | Acc]);
expr_elem([$\r, $\n | T], Depth, ExprLn, StrLn, Acc) ->
    expr_elem(T, Depth, ExprLn, StrLn + 1, [$\n, $\r | Acc]);
expr_elem([$\r | T], Depth, ExprLn, StrLn, Acc) ->
    expr_elem(T, Depth, ExprLn, StrLn + 1, [$\r | Acc]);
expr_elem([$\n | T], Depth, ExprLn, StrLn, Acc) ->
    expr_elem(T, Depth, ExprLn, StrLn + 1, [$\n | Acc]);
expr_elem([H | T], Depth, ExprLn, StrLn, Acc) ->
    expr_elem(T, Depth, ExprLn, StrLn, [H | Acc]);
expr_elem([], _Depth, ExprLn, _StrLn, _Acc) ->
    _Anno = erl_anno:new(ExprLn),
    error("Unterminated interpolation expression in ~f string. Expected '}'.").

str_tuple(Ln, Acc) ->
    Anno = erl_anno:new(Ln),
    Str = lists:reverse(Acc),
    {string, Anno, Str}.

block_tuple(Ln, Acc) ->
    Anno = erl_anno:new(Ln),
    Expr = lists:reverse([$. | Acc]),
    {ok, Tokens, _} = erl_scan:string(Expr),
    {ok, Forms0} = erl_parse:parse_exprs(Tokens),
    Forms = erl_parse:map_anno(fun(_Anno) -> Anno end, Forms0),
    Block = erl_syntax:revert(erl_syntax:set_pos(erl_syntax:block_expr(Forms), Anno)),
    {block, Anno, Block}.
