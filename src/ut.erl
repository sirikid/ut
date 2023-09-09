-module(ut).

-include("ut_records.hrl").

%% Records

-record(ut_template, {components}).

%% Types

-export_type([template/0, parse_opt/0, expand_opt/0]).

-export_type([expression/0, variable/0, path/0, key/0, modifier/0]).

-opaque template() :: #ut_template{components :: [component()]}.

-type parse_opt() ::
        {variables, binary | atoms | 'atoms!'}
      | {dots, keep | split}.

-type expand_opt() :: {binary, boolean()} | ut_expr:expand_opt().

%% Internal types

-type component() :: literal() | expression().

-type literal() :: binary().

-type expression() :: {operator(), [variable()]}.

-type operator() :: simple | reserved | labels | path | parameter | query | query_cont | fragment.

-type variable() :: #ut_var{name :: binary(), path :: path(), modifier :: modifier()}.

-type path() :: [key(), ...].

-type key() :: atom() | binary().

-type modifier() :: none | exploded | {trim, pos_integer()}.

%% Functions

-export(['parse!'/1, 'parse!'/2, parse/1, parse/2, expand/2, expand/3]).

%% @equiv 'parse!'(String, [])
-spec 'parse!'(string()) -> template() | no_return().
'parse!'(String) ->
    'parse!'(String, []).

-spec 'parse!'(string(), [parse_opt()]) -> template() | no_return().
'parse!'(String, Options) ->
    {ok, Template} = parse(String, Options),
    Template.

%% @equiv parse(String, [])
-spec parse(string()) -> {ok, template()} | {error, term()}.
parse(String) ->
    parse(String, []).

-spec parse(string(), [parse_opt()]) -> {ok, template()} | {error, term()}.
parse(String, Options) ->
    case ut_parser:parse(String) of
        {fail, Fail} ->
            {error, Fail};
        Components1 ->
            Variables = proplists:get_value(variables, Options, binary),
            Dots = proplists:get_value(dots, Options, keep),

            MapF =
                case Variables of
                    binary -> fun(Key) -> Key end;
                    atoms -> fun binary_to_atom/1;
                    'atoms!' -> fun binary_to_existing_atom/1
                end,
            ConvF =
                case Dots of
                    split -> fun(Path) -> lists:map(MapF, Path) end;
                    keep -> fun(Path) -> [MapF(unicode:characters_to_binary(lists:join(<<".">>, Path)))] end
                end,
            Components2 = transform_expressions(Components1, fun ut_path:conv/3, [ConvF]),
            {ok, #ut_template{components=Components2}}
    end.

-spec expand(template() | string(), map()) -> iodata().
expand(Template, Substitutes) ->
    expand(Template, Substitutes, []).

-spec expand(template(), map(), [expand_opt()]) -> iodata();
            (string(), map(), [parse_opt() | expand_opt()]) -> iodata().
expand(#ut_template{components=Components}, Substitutes, Options) ->
    Expansion = transform_expressions(Components, fun ut_expr:expand/4, [Substitutes, Options]),
    case proplists:get_value(binary, Options, true) of
        false ->
            Expansion;
        true ->
            iolist_to_binary(Expansion)
    end;
expand(String, Substitutes, Options) ->
    {ok, Template} = parse(String, Options),
    expand(Template, Substitutes, Options).

%% Internal functions

-spec transform_expressions([component()], function(), list()) -> list().
transform_expressions(Components, Fun, ExtraArgs) ->
    [case LitOrExpr of
         <<Lit/binary>> ->
             Lit;
         {Op, Variables} ->
             apply(Fun, [Op, Variables | ExtraArgs])
     end || LitOrExpr <- Components].
