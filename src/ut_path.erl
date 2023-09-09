-module(ut_path).

-include("ut_records.hrl").

%% Types

-export_type([conv/0, conv_fun/0]).

-type conv() :: none | binary | atoms | 'atoms!' | conv_fun().

-type conv_fun() :: fun((ut:path()) -> ut:path()).

%% Functions

-export([conv/3, conv_fun/1, key_conv/1]).

-spec conv(atom(), [ut:variable()], conv()) -> {atom(), [ut:variable()]}.
conv(Op, Vars, Conv) ->
    ConvF = conv_fun(Conv),
    {Op, [Var#ut_var{path = ConvF(Var#ut_var.path)} || Var <- Vars]}.

-spec conv_fun(conv()) -> conv_fun().
conv_fun(none) ->
    fun(Path) -> [iolist_to_binary(lists:join(<<".">>, Path))] end;
conv_fun(binary) ->
    key_conv(fun erlang:atom_to_binary/1);
conv_fun(atoms) ->
    key_conv(fun erlang:binary_to_atom/1);
conv_fun('atoms!') ->
    key_conv(fun erlang:binary_to_existing_atom/1);
conv_fun(Fun) when is_function(Fun, 1) ->
    Fun.

-spec key_conv(fun((ut:key()) -> ut:key())) -> conv_fun().
key_conv(KeyF) when is_function(KeyF, 1) ->
    fun(Path) -> lists:map(KeyF, Path) end.
