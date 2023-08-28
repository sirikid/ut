-module(ut).

-export([parse/1, parse/2]).

-export_type([t/0, parse_opts/0]).

%% Types

-opaque t() :: [literal() | expression()].

-type literal() :: binary().

-type expression() :: {atom(), [variable()]}.

-type variable() :: variable_name() | {variable_name(), exploded | pos_integer()}.

-type variable_name() :: variable_name(atom() | binary()).

-type variable_name(T) :: [T, ...] | T.

%% Opts

-type parse_opts() :: [parse_opt()].

-type parse_opt() :: {variables, variable_conv()}.

-type variable_conv() :: strings | atoms | 'atoms!' | variable_conv_fun().

-type variable_conv_fun() :: fun((variable_name()) -> variable_name()).

%% Functions

-spec parse(Template :: binary()) -> {ok, t()} | {error, term()}.
parse(Template) ->
    parse(Template, []).

-spec parse(Template :: binary(), parse_opts()) -> {ok, t()} | {error, term()}.
parse(Template, Options) ->
    try ut_parser:parse(Template) of
        {fail, Fail} ->
            {error, Fail};
        T1 when is_list(T1) ->
            Conv = proplists:get_value(variables, Options, strings),
            T2 = transform_template(T1, conv_to_fun(Conv)),
            {ok, T2}
    catch
        Class:Exception:Trace ->
            {error, {Class, Exception, Trace}}
    end.

-spec conv_to_fun(variable_conv()) -> variable_conv_fun().
conv_to_fun(strings) ->
    fun(<<Str/binary>>) -> Str end;
conv_to_fun(atoms) ->
    fun erlang:binary_to_atom/1;
conv_to_fun('atoms!') ->
    fun erlang:binary_to_existing_atom/1;
conv_to_fun(Fun) when is_function(Fun, 1) ->
    Fun.

-spec transform_template(t(), variable_conv_fun()) -> t().
transform_template(T, ConvF) ->
    [transform_expression(LitOrExpr, ConvF) || LitOrExpr <- T].

-spec transform_expression(literal() | expression(), variable_conv_fun()) -> literal() | expression().
transform_expression(<<Lit/binary>>, _ConvF) ->
    Lit;
transform_expression({Op, Variables}, ConvF) ->
    {Op, [transform_variable(Variable, ConvF) || Variable <- Variables]}.

-spec transform_variable(variable(), variable_conv_fun()) -> variable_name().
transform_variable({Name, Modifier}, ConvF) ->
    {transform_name(Name, ConvF), Modifier};
transform_variable(Name, ConvF) ->
    transform_name(Name, ConvF).

-spec transform_name(variable_name(), variable_conv_fun()) -> variable_name().
transform_name(<<Name/binary>>, ConvF) ->
    ConvF(Name);
transform_name([_|_] = Name, ConvF) ->
    [ConvF(Key) || Key <- Name].
