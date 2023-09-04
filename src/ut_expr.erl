-module(ut_expr).

-include("ut_records.hrl").

%% Macros

-define(is_empty(Value), Value == <<>>; Value == []; Value == #{}).

-define(is_scalar(Value), is_atom(Value); is_number(Value); is_binary(Value)).

%% Records

-record(op_desc, {first, sep, named=false, ifemp="", encode=fun uri_string:quote/1}).

%% Types

%% Internal types

%% Functions

-export([expand/3]).

-spec expand(ut:operator(), [ut:variable()], ut:substitutes()) -> iodata().
expand(Operator, Variables, Substitutes) ->
    Desc = operator_description(Operator),
    GetValueF = fun(Var) -> ut_value:get(Var, Substitutes, fun ut_value:ignore/2) end,
    do_expand(Desc, lists:filtermap(GetValueF, Variables)).

%% Internal functions

-spec operator_description(atom()) -> #op_desc{}.
operator_description(Operator) ->
    case Operator of
        simple     -> #op_desc{first="", sep=","};
        reserved   -> #op_desc{first="", sep=",", encode=fun quote_but_reserved/1};
        labels     -> #op_desc{first=".", sep="."};
        path       -> #op_desc{first="/", sep="/"};
        parameter  -> #op_desc{first=";", sep=";", named=true};
        query      -> #op_desc{first="?", sep="&", named=true, ifemp="="};
        query_cont -> #op_desc{first="&", sep="&", named=true, ifemp="="};
        fragment   -> #op_desc{first="#", sep=",", encode=fun quote_but_reserved/1}
    end.

%% proplists:get_value(reserved, uri_string:allowed_characters()).
-spec quote_but_reserved(atom() | binary() | number()) -> binary().
quote_but_reserved(<<String/binary>>) ->
    uri_string:quote(String, "!#$&'()*+,/:;=?@[]").

%% Why we have to write this function every time?
-spec to_binary(atom() | number() | binary()) -> binary().
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
to_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
to_binary(Float) when is_float(Float) ->
    float_to_binary(Float);
to_binary(<<Binary/binary>>) ->
    Binary.

-spec do_expand(#op_desc{}, [#ut_value{}]) -> iodata().
do_expand(_Desc, []) ->
    [];
do_expand(#op_desc{first=First, sep=Sep}=Desc, [_|_]=Values) ->
    [First | map_join(Sep, fun(Value) -> expand_value(Desc, Value) end, Values)].

-spec expand_value(#op_desc{}, #ut_value{}) -> iodata().
expand_value(#op_desc{named=false}, #ut_value{value=Empty}) when ?is_empty(Empty) ->
    [];
expand_value(#op_desc{named=true, ifemp=IfEmp, encode=Encode}, #ut_value{name=Name, value=Empty}) when ?is_empty(Empty) ->
    [Encode(Name), IfEmp];
expand_value(#op_desc{named=false, encode=Encode}, #ut_value{value=Scalar, modifier=none}) when ?is_scalar(Scalar) ->
    Encode(to_binary(Scalar));
expand_value(#op_desc{named=true, encode=Encode}, #ut_value{name=Name, value=Scalar, modifier=none}) when ?is_scalar(Scalar) ->
    [Encode(Name), <<"=">>, Encode(to_binary(Scalar))];
expand_value(#op_desc{named=false, encode=Encode}, #ut_value{value=Scalar, modifier={trim, Length}}) when ?is_scalar(Scalar) ->
    Encode(string:slice(to_binary(Scalar), 0, Length));
expand_value(#op_desc{named=true, encode=Encode}, #ut_value{name=Name, value=Scalar, modifier={trim, Length}}) when ?is_scalar(Scalar) ->
    [Encode(Name), <<"=">>, Encode(string:slice(to_binary(Scalar), 0, Length))];
expand_value(#op_desc{named=false, encode=Encode}, #ut_value{value=[_|_]=List, modifier=none}) ->
    map_join(<<",">>, Encode, List);
expand_value(#op_desc{named=true, encode=Encode}, #ut_value{name=Name, value=[_|_]=List, modifier=none}) ->
    [Encode(Name), <<"=">> | map_join(<<",">>, Encode, List)];
expand_value(#op_desc{named=false, sep=Sep, encode=Encode}, #ut_value{value=[_|_]=List, modifier=exploded}) ->
    map_join(Sep, Encode, List);
expand_value(#op_desc{named=true, sep=Sep, encode=Encode}, #ut_value{name=Name, value=[_|_]=List, modifier=exploded}) ->
    map_join(Sep, fun(Value) -> [Encode(Name), <<"=">>, Encode(Value)] end, List);
expand_value(#op_desc{named=false, encode=Encode}, #ut_value{value=#{}=Map, modifier=none}) when map_size(Map) > 0 ->
    map_join(<<",">>, fun({Key, Value}) -> [Encode(Key), <<",">>, Encode(Value)] end, to_list(Map));
expand_value(#op_desc{named=true, encode=Encode}, #ut_value{name=Name, value=#{}=Map, modifier=none}) when map_size(Map) > 0 ->
    [Encode(Name), <<"=">> | map_join(<<",">>, fun({Key, Value}) -> [Encode(Key), <<",">>, Encode(Value)] end, to_list(Map))];
expand_value(#op_desc{sep=Sep, encode=Encode}, #ut_value{value=#{}=Map, modifier=exploded}) when map_size(Map) > 0 ->
    map_join(Sep, fun({Key, Value}) -> [Encode(Key), <<"=">>, Encode(Value)] end, to_list(Map)).

-spec map_join(Sep, fun((T) -> R), [T]) -> [Sep | R].
map_join(Sep, Fun, List) ->
    lists:join(Sep, lists:map(Fun, List)).

-spec to_list(map()) -> [{term(), term()}].
to_list(Map) ->
    lists:keysort(1, maps:to_list(Map)).