-module(ut_value).

-include("ut_records.hrl").

%% Records

-record(ut_key_error, {valid_path, key, rest_path, substitutes}).

%% Internal types

-type error_fun() :: fun((ut:variable(), term()) -> no_return() | false | {true, value()}).

-type substitutes() :: #{ut:key() => value()}.

-type value() :: scalar() | [substitutes()] | #{ut:key() => value()}.

-type scalar() :: atom() | number() | binary().

%% Functions

-export([get/3, get_fun/2, require_keys_fun/2, try_default_fun/2, ignore_error/2, throw_error/2]).

-spec get(ut:variable(), substitutes(), function()) -> false | {true, value()}.
get(#ut_var{name=Name, path=Path, modifier=Modifier}=Var, Substitutes, ErrorF) ->
    case do_get(Path, [], Substitutes) of
        {error, Reason} ->
            ErrorF(Var, Reason);
        {ok, Value} ->
            {true, #ut_value{name=Name, value=Value, modifier=Modifier}}
    end.

-spec get_fun(substitutes(), error_fun()) -> function().
get_fun(Substitutes, ErrorF) ->
    fun(Var) -> get(Var, Substitutes, ErrorF) end.

-spec require_keys_fun([ut:key()], error_fun()) -> error_fun().
require_keys_fun(RequiredKeys, NextF) ->
    fun(#ut_var{}=Var, #ut_key_error{key=Key}=Error) ->
            case lists:member(Key, RequiredKeys) of
                false ->
                    NextF(Var, Error);
                true ->
                    throw_error(Var, Error)
            end
    end.

-spec try_default_fun(substitutes(), error_fun()) -> error_fun().
try_default_fun(Defaults, NextF) ->
    fun(#ut_var{}=Var, Error) ->
            get(Var, Defaults, fun(_Var, _Error) -> NextF(Var, Error) end)
    end.

-spec ignore_error(ut:variable(), #ut_key_error{}) -> false.
ignore_error(_Var, _Error) -> false.

-spec throw_error(ut:variable(), #ut_key_error{}) -> no_return().
throw_error(#ut_var{name=Name}, #ut_key_error{key=Key}) ->
    erlang:error({undefined_variable, Name, Key}).

%% Internal functions

-spec do_get([ut:key()], [ut:key()], substitutes()) -> {ok, value()} | {error, #ut_key_error{}}.
do_get([], _Path, Value) ->
    {ok, Value};
do_get([Key|Keys], Path, Substitutes) ->
    case Substitutes of
        #{Key := Value} when Value =/= null ->
            do_get(Keys, [Key|Path], Value);
        _ ->
            Reason = #ut_key_error{valid_path=lists:reverse(Path), key=Key, rest_path=Keys, substitutes=Substitutes},
            {error, Reason}
    end.
