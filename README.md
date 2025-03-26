# custom_sigils

This is a POC of custom sigils implementation for Erlang/OTP.

It is based on [this comment](https://erlangforums.com/t/proposal-introduce-f-sigils-for-string-interpolation/4551/21?u=williamthome)
on the Erlang Forums.

This is a minimal change and does not break any existing code.

## How it works

The developer can use the built-in sigils, like `s`, `S`, `b`, and `B`, but it
can also use any sigil it wants. However, it is obligated to handle the custom
sigil with parse transform, otherwise, an `invalid sigil` exception will be raised
at the compile time.

### Why parse transform?

It was the first idea and seems to be the simplest one to implement this feature.

## Examples

There are two functions of custom sigils implemented in this project:

- `json`: Decodes a JSON into an Erlang term via `json:decode/1`
- `sql`: Transforms a SQL query into a tuple based on the `f_sigil` proposal.
  This sigil extracts the params and replaces them with `$N`, where N is the
  param index of that param.

### The code of the `example` module

```erlang
-module(example).
-compile({parse_transform, json_sigil}).
-compile({parse_transform, sql_sigil}).
%% Sigils can be defined globally in rebar3.config (applies to all .erl files), e.g.:
%{erl_opts, [
%    {parse_transform, json_sigil},
%    {parse_transform, sql_sigil}
%]}.
-export([json/0]).
-export([sql/2]).

json() ->
    ~json"""
    {"key": "value"}
    """.

sql(Name, MinAge) when is_binary(Name); is_integer(MinAge) ->
    ~sql"""
    SELECT *
      FROM users
     WHERE name = {Name}
       AND age >= {MinAge}
    """.

%% Uncomment this function to raise an invalid sigil exception.
%invalid() ->
%    ~invalid"".
```

### The `json_sigil` code

```erlang
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
```

### The `sql_sigil` code

Please refer to the project module for the complete code.

### Running the examples

```erlang
1> c(json_sigil), c(sql_sigil), c(example).
{ok,example}
2> example:json().
#{<<"key">> => <<"value">>}
3> example:sql(~"Bob", 18).
{sql,<<"SELECT *\n  FROM users\n WHERE name = $1\n   AND age >= $2">>,
     [<<"Bob">>,18]}
4> ~invalid"".
* 1:1: invalid sigil
```
