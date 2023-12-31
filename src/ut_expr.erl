-module(ut_expr).

-include("ut_records.hrl").

%% Macros

-define(is_empty(Value), Value == <<>>; Value == []; Value == #{}; Value == '').

-define(is_scalar(Value), is_binary(Value); is_number(Value); is_atom(Value)).

%% Records

-record(op_desc, {first, sep, named=false, ifemp="", allow=unreserved}).

%% Types

-export_type([expand_opt/0]).

-type expand_opt() :: {ignore_missing_keys, boolean()} | {required_keys, [ut:key()]} | {defaults, ut_value:substitutes()}.

%% Functions

-export([expand/4]).

-spec expand(ut:operator(), [ut:variable()], ut:substitutes(), [expand_opt()]) -> iodata().
expand(Operator, Variables, Substitutes, Options) ->
    Desc = operator_description(Operator),
    IgnoreMissingKeys = proplists:get_value(ignore_missing_keys, Options, true),
    RequiredKeys = proplists:get_value(required_keys, Options, []),
    Defaults = proplists:get_value(defaults, Options, #{}),
    ErrorF1 =
        case IgnoreMissingKeys of
            true -> fun ut_value:ignore_error/2;
            false -> fun ut_value:throw_error/2
        end,
    ErrorF2 = ut_value:try_default_fun(Defaults, ErrorF1),
    ErrorF3 = ut_value:require_keys_fun(RequiredKeys, ErrorF2),
    GetValueF = ut_value:get_fun(Substitutes, ErrorF3),
    do_expand(Desc, lists:filtermap(GetValueF, Variables)).

%% Internal functions

-spec operator_description(atom()) -> #op_desc{}.
operator_description(Operator) ->
    case Operator of
        simple     -> #op_desc{first="", sep=","};
        reserved   -> #op_desc{first="", sep=",", allow=reserved};
        labels     -> #op_desc{first=".", sep="."};
        path       -> #op_desc{first="/", sep="/"};
        parameter  -> #op_desc{first=";", sep=";", named=true};
        query      -> #op_desc{first="?", sep="&", named=true, ifemp="="};
        query_cont -> #op_desc{first="&", sep="&", named=true, ifemp="="};
        fragment   -> #op_desc{first="#", sep=",", allow=reserved}
    end.

%% Why we have to write this function every time?
-spec to_binary(atom() | number() | binary()) -> binary().
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
to_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
to_binary(Float) when is_float(Float) ->
    float_to_binary(Float, [short]);
to_binary(<<Binary/binary>>) ->
    Binary.

-spec do_expand(#op_desc{}, [#ut_value{}]) -> iodata().
do_expand(#op_desc{first=First, sep=Sep}=Desc, Values) ->
    case map_join(Sep, fun(Value) -> expand_value(Desc, Value) end, Values) of
        [] -> [];
        NonEmpty -> [First | NonEmpty]
    end.

-spec expand_value(#op_desc{}, #ut_value{}) -> iodata().
expand_value(#op_desc{named=false}, #ut_value{value=Empty}) when ?is_empty(Empty) ->
    [];
expand_value(#op_desc{named=true, ifemp=IfEmp}, #ut_value{name=Name, value=Empty}) when ?is_empty(Empty) ->
    [encode(Name, name), IfEmp];
expand_value(#op_desc{named=false, allow=Allow}, #ut_value{value=Scalar, modifier=none}) when ?is_scalar(Scalar) ->
    encode(to_binary(Scalar), Allow);
expand_value(#op_desc{named=true, allow=Allow}, #ut_value{name=Name, value=Scalar, modifier=none}) when ?is_scalar(Scalar) ->
    [encode(Name, name), <<"=">>, encode(to_binary(Scalar), Allow)];
expand_value(#op_desc{named=false, allow=Allow}, #ut_value{value=Scalar, modifier={trim, Length}}) when ?is_scalar(Scalar) ->
    encode(string:slice(to_binary(Scalar), 0, Length), Allow);
expand_value(#op_desc{named=true, allow=Allow}, #ut_value{name=Name, value=Scalar, modifier={trim, Length}}) when ?is_scalar(Scalar) ->
    [encode(Name, name), <<"=">>, encode(string:slice(to_binary(Scalar), 0, Length), Allow)];
expand_value(#op_desc{allow=Allow}, #ut_value{value=Scalar, modifier=exploded}) when ?is_scalar(Scalar) ->
    [encode(to_binary(Scalar), Allow)];
expand_value(#op_desc{named=false, allow=Allow}, #ut_value{value=[_|_]=List, modifier=none}) ->
    map_join(<<",">>, fun(Value) -> encode(Value, Allow) end, List);
expand_value(#op_desc{named=true, allow=Allow}, #ut_value{name=Name, value=[_|_]=List, modifier=none}) ->
    [encode(Name, name), <<"=">> | map_join(<<",">>, fun(Value) -> encode(Value, Allow) end, List)];
expand_value(#op_desc{named=false, sep=Sep, allow=Allow}, #ut_value{value=[_|_]=List, modifier=exploded}) ->
    map_join(Sep, fun(Value) -> encode(Value, Allow) end, List);
expand_value(#op_desc{named=true, sep=Sep, allow=Allow}, #ut_value{name=Name, value=[_|_]=List, modifier=exploded}) ->
    map_join(Sep, fun(Value) -> [encode(Name, name), <<"=">>, encode(Value, Allow)] end, List);
expand_value(#op_desc{named=false, allow=Allow}, #ut_value{value=#{}=Map, modifier=none}) when map_size(Map) > 0 ->
    map_join(<<",">>, fun({Key, Value}) -> [encode(Key, name), <<",">>, encode(Value, Allow)] end, to_list(Map));
expand_value(#op_desc{named=true, allow=Allow}, #ut_value{name=Name, value=#{}=Map, modifier=none}) when map_size(Map) > 0 ->
    [encode(Name, name), <<"=">> | map_join(<<",">>, fun({Key, Value}) -> [encode(Key, name), <<",">>, encode(Value, Allow)] end, to_list(Map))];
expand_value(#op_desc{sep=Sep, allow=Allow}, #ut_value{value=#{}=Map, modifier=exploded}) when map_size(Map) > 0 ->
    map_join(Sep, fun({Key, Value}) -> [encode(Key, name), <<"=">>, encode(Value, Allow)] end, to_list(Map)).

-spec map_join(Sep, fun((T) -> R), [T]) -> [Sep | R].
map_join(Sep, Fun, List) ->
    lists:join(Sep, lists:map(Fun, List)).

-spec to_list(map()) -> [{term(), term()}].
to_list(Map) ->
    lists:keysort(1, maps:to_list(Map)).

-spec encode(binary(), atom()) -> binary().
encode(String, unreserved) ->
    uri_string:quote(String);
encode(String, name) ->
    preserve_percent_encoded(String, "");
encode(String, reserved) ->
    preserve_percent_encoded(String, "!#$&'()*+,/:;=?@[]").

preserve_percent_encoded(String, Allow) ->
    [case Part of
         {literal, Literal} ->
             uri_string:quote(Literal, Allow);
         {encoded, Encoded} ->
             Encoded
     end || Part <- find_percent_encoded(String)].

find_percent_encoded(String) ->
    case re:run(String, "%[[:xdigit:]]{2}", [global]) of
        nomatch ->
            [{literal, String}];
        {match, Matches} ->
            {_, [_Fake | Result]} =
                lists:foldl(
                  fun([{Pos, Length}], {EndOfPrevMatch, Acc}) ->
                          Literal = binary:part(String, EndOfPrevMatch, Pos-EndOfPrevMatch),
                          PctEncoded = binary:part(String, Pos, Length),
                          {Pos+Length, [{encoded, PctEncoded}, {literal, Literal} | Acc]}
                  end,
                  {0, []},
                  Matches ++ [[{byte_size(String), 0}]]),
            lists:reverse(Result)
    end.
