-module(ut_value).

%% Includes

-include("ut_records.hrl").

%% Records

%% Types

%% Internal types

-type error_fun() :: fun((ut:variable(), term()) -> no_return() | false | {true, value()}).

-type substitutes() :: #{ut:key() => value()}.

-type value() :: scalar() | [substitutes()] | #{ut:key() => value()}.

-type scalar() :: atom() | number() | binary().

%% Functions

-export([get/3, ignore/2, error/2]).

-spec get(ut:variable(), substitutes(), function()) -> false | {true, value()}.
get(#ut_var{name=Name, path=Path, modifier=Modifier}=Var, Substitutes, ErrorF) ->
    case do_get(Path, [], Substitutes) of
        {error, Reason} ->
            ErrorF(Var, Reason);
        {ok, Value} ->
            {true, #ut_value{name=Name, value=Value, modifier=Modifier}}
    end.

-spec ignore(ut:variable(), term()) -> false.
ignore(_Var, _Error) ->
    false.

-spec error(ut:variable(), term()) -> no_return().
error(_Var, _Error) ->
    erlang:error({undefined}).

%% Internal functions

-spec do_get([ut:key()], [ut:key()], substitutes()) -> {ok, value()} | {error, {[ut:key()], ut:key(), [ut:key()], substitutes()}}.
do_get([], _Path, Value) -> {ok, Value};
do_get([Key|Keys], Path, Substitutes) ->
    case Substitutes of
        #{Key := Value} ->
            do_get(Keys, [Key|Path], Value);
        _ ->
            {error, {lists:reverse(Path), Key, Keys, Substitutes}}
    end.
