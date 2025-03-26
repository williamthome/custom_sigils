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
