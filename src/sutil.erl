%%% @author Vladimir G. Sekissov <>
%%% @copyright (C) 2013, Vladimir G. Sekissov
%%% @doc Утилиты.
%%%
%%% @end
%%% Created : 10 Dec 2013 by Vladimir G. Sekissov <>

-module(sutil).

-compile({inline, [hex/1]}).

-export([start_app_deps/1, run_jobs/1]).
-export([wait_for/1]).
-export([curry/2, rcurry/2]).
-export(['->'/2, '->>'/2]).
-export([from_maybe/2, 'maybe->'/2, 'maybe->>'/2, maybe/3, maybe_do/1, maybe_lift/1, maybe_unlift/1]).
-export([maybe_collect/1]).
-export([hexstr/1, ensure_binary/1, ensure_list/1, ensure_integer/1]).
-export([proplist_validate/4, proplist_extract/2, proplist_require/2, proplist_defaults/2]).
-export([records_merge/2, proplist_merge/2]).
-export([glob_to_regexp/1]).

-export_type([maybe/2]).

-include("../include/sutil.hrl").

-type maybe(Ok, Error) :: {ok, Ok} | {error, Error}.

%% @doc Runs _Funs_ in parallel and returns results
%% @end
-spec run_jobs(Funs::[function()]) -> [term()|{error, term()}].
run_jobs(Funs) ->
    Self = self(),
    [wait_job_result(W) || W <- [spawn_job_worker(Self, F) || F <- Funs]].

spawn_job_worker(Parent, Fun) ->
    ExecF = case Fun of
                {M, F, A} ->
                    fun () -> erlang:apply(M, F, A) end;
                _ when is_function(Fun, 0) ->
                    Fun
            end,
    erlang:spawn_monitor(fun() -> Parent ! {self(), ExecF()} end).

wait_job_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid, Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> {error, Reason}
    end.

%% @doc start application with its dependencies
%% @end
-spec start_app_deps(App::atom()) -> [StartedApplication::atom()].
start_app_deps(App) ->
    start_app_deps(App, []).

start_app_deps(App, Started) ->
    case application:load(App) of
        ok ->
            ok;
        {error, {already_loaded, App}} ->
            ok;
        Error ->
            Error
    end,
    DepApps = case application:get_key(App, applications) of
                  undefined ->
                      [];
                  {ok, V} ->
                      V
              end,
    DepApps1 = ordsets:from_list(DepApps),
    ToStart = ordsets:subtract(DepApps1, Started),
    Started1 = lists:foldl(fun (A, Acc) ->
                                   start_app_deps(A, Acc)
                           end, Started, ToStart),
    ensure_started(App),
    ordsets:union(Started1, DepApps1).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

wait_for(Pid) ->
    MRef= erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, _, _} ->
            ok
    end.

curry(Fn, Args) ->
    fun (X) ->
            apply(Fn, [X|Args])
    end.

rcurry(Fn, Args) ->
    fun (X) ->
            apply(Fn, Args ++ [X])
    end.

%% @doc Call sequence of functions with result of a previous as first argument
%% for a next
%% @end
-spec '->'(X::term(), Fns::list(fun())) -> any() | no_return().
'->'(X, Fs) ->
    lists:foldl(fun({F, A}, Acc) -> apply(F, [Acc|A]);
                   ({M, F, A}, Acc) -> apply(M, F, [Acc|A]);
                   (F, Acc) when is_function(F, 1) -> F(Acc)
                end, X, Fs).

%% @doc Call sequence of functions with result of a previous as last argument
%% for a next
%% @end
-spec '->>'(X::term(), Fs::list(fun())) -> maybe(_, _).
'->>'(X, Fs) ->
    lists:foldl(fun({F, A}, Acc) -> apply(F, A ++ [Acc]);
                   ({M, F, A}, Acc) -> apply(M, F, A ++ [Acc]);
                   (F, Acc) when is_function(F, 1) -> F(Acc)
                end, X, Fs).

%% @doc Returns value if value is not _undefined_ or default
%% @end
from_maybe(undefined, Val) -> Val;
from_maybe(Val, _) -> Val.

%% @doc Call sequence of functions return result of the last.
%% @end
-spec maybe_do(Fns::list(fun())) -> maybe(_, _).
maybe_do(Fs) ->
    reduce(fun ({M, F, A}, _) -> apply(M, F, A);
               ({F, A}, _) when is_function(F, length(A)) -> apply(F, A);
               (F, _) when is_function(F, 0) -> F()
           end, [], Fs).

%% @doc Call sequence of functions return results.
%% @end
-spec maybe_collect(Fns::list(fun())) -> [term()] | {error, term()}.
maybe_collect(Fs) ->
    case reduce(fun ({M, F, A}, Acc) -> [maybe_unlift_(apply(M, F, A))|Acc];
                    ({F, A}, Acc) -> [maybe_unlift_(apply(F, A))|Acc];
                    (F, Acc) when is_function(F, 0) -> [maybe_unlift_(F())|Acc]
                end, [], Fs) of
        {ok, Ret} ->
            lists:reverse(Ret);
        Error ->
            Error
    end.

%% @doc Call sequence of functions with result of a previous as first argument
%% for a next
%% @end
-spec 'maybe->'(X::term(), Fns::list(fun())) -> maybe(_, _).
'maybe->'(X, Fs) ->
    reduce(fun({F, A}, Acc) -> apply(F, [Acc|A]);
              ({M, F, A}, Acc) -> apply(M, F, [Acc|A]);
              (F, Acc) when is_function(F, 1) -> F(Acc)
           end, X, Fs).

%% @doc Call sequence of functions with result of a previous as last argument
%% for a next
%% @end
-spec 'maybe->>'(X::term(), Fs::list(fun())) -> maybe(_, _).
'maybe->>'(X, Fs) ->
    reduce(fun({F, A}, Acc) -> apply(F, A ++ [Acc]);
              ({M, F, A}, Acc) -> apply(M, F, A ++ [Acc]);
              (F, Acc) when is_function(F, 1) -> F(Acc)
           end, X, Fs).

-spec maybe(F::fun(), Succ::fun(), Fail::fun()) -> maybe(_, _).
%% @doc Lift `F' into maybe(), then apply `Ok' or `Error' to the resulting
%% value.
maybe(Fn, Ok, Error) ->
    case maybe_lift(Fn) of
        {ok, Res} -> ?maybe_lift(Ok(Res));
        {error, Rsn} -> ?maybe_lift(Error(Rsn))
    end.

reduce(F, Acc0, Xs) ->
    ?maybe_lift(lists:foldl(fun(X, Acc) -> ?maybe_unlift(F(X, Acc)) end, Acc0, Xs)).

-spec maybe_lift(fun()) -> maybe(_, _).
maybe_lift({F, A}) ->
    maybe_lift(fun () -> apply(F, A) end);
maybe_lift(F) ->
    try F() of
        ok -> {ok, ok};
        {ok, Res} -> {ok, Res};
        error -> {error, error};
        {error, Rsn} -> {error, Rsn};
        Res -> {ok, Res}
    catch
        throw:{error, Rsn} -> {error, Rsn};
        _:Exn -> {error, {lifted_exn, Exn, erlang:get_stacktrace()}}
    end.

-spec maybe_unlift(fun()) -> _.
maybe_unlift({F, A}) ->
    maybe_unlift_(apply(F, A));
maybe_unlift(F) ->
    maybe_unlift_(F()).

maybe_unlift_(Res) ->
    case Res of
        ok -> ok;
        {ok, Ret} -> Ret;
        error -> throw({error, error});
        {error, Rsn} -> throw({error, Rsn});
        Res -> Res
    end.

%% @doc Hierarchically merge two records of the same type
records_merge(To, From) ->
    SameRecF= fun (R1, R2) when element(1, R1) == element(1, R2),
                                size(R1) == size(R2) ->
                      true;
                  (_, _) ->
                      false
              end,
    MergeFieldsF = fun (undefined, F2) ->
                           F2;
                       (F1, F2) when is_tuple(F1),
                                     is_tuple(F2) ->
                           case SameRecF(F1, F2) of
                               true -> records_merge(F1, F2);
                               false -> F1
                           end;
                       (F1, _) ->
                           F1
                   end,

    true = SameRecF(To, From),
    list_to_tuple([MergeFieldsF(F1, F2) ||
                      {F1, F2} <- lists:zip(tuple_to_list(To), tuple_to_list(From))]).

proplist_merge(PL, DefaultPL) ->
    lists:ukeymerge(1, lists:usort(PL), lists:usort(DefaultPL)).

proplist_validate(PL, Required, Validators, Defaults) ->
    proplist_require(Required, PL),
    PL1 =
        [case proplists:get_value(K, Validators) of
             undefined ->
                 {K, V};
             VF ->
                 try
                     {K, VF(V)}
                 catch
                     _:Error ->
                         throw({error, {validation, {K, V}, Error, erlang:get_stacktrace()}})
                 end
         end || {K, V} <- PL],
    PL2 = proplist_defaults(Defaults, PL1),
    PL2.

-spec proplist_extract(Keys::[deeprops:path()], Props::deeprops:proplist()) ->
                              {[term()], deeprops:proplist()}.
proplist_extract(Keys, Props) ->
    lists:mapfoldl(fun deepprops:extract/2, Props, Keys).

proplist_require(Keys, PL) ->
    Undef = make_ref(),
    [case proplists:get_value(Key, PL, Undef) of
         Undef ->
             throw({error, {Key, required}});
         Val ->
             Val
     end || Key <- Keys].

proplist_defaults(Defaults, PL) ->
    [{K, V} || {K, V} <- Defaults, proplists:is_defined(K, PL) == false] ++ PL.

ensure_binary(I) when is_integer(I) ->
    erlang:integer_to_binary(I);
ensure_binary(F) when is_float(F) ->
    erlang:float_to_binary(F, [{decimals, 6}, compact]);
ensure_binary(A) when is_atom(A) ->
    erlang:atom_to_binary(A, utf8);
ensure_binary(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary(B) when is_binary(B) ->
    B.

ensure_list(A) when is_atom(A) ->
    atom_to_list(A);
ensure_list(L) when is_list(L) ->
    L;
ensure_list(B) when is_binary(B) ->
    binary_to_list(B).

ensure_integer(I) when is_integer(I) ->
    I;
ensure_integer(Str) when is_binary(Str) ->
    erlang:binary_to_integer(ensure_binary(Str));
ensure_integer(Str) when is_list(Str) ->
    list_to_integer(Str).

glob_to_regexp(Str) when is_binary(Str) ->
    list_to_binary(glob_to_regexp(binary_to_list(Str)));
glob_to_regexp(Str) ->
    glob_to_regexp(Str, [$^]).

glob_to_regexp([], Acc) ->
    {ok, Ret} = re:compile(lists:reverse([$\$|Acc])),
    Ret;
glob_to_regexp([$\\,C|Rest], Acc) ->
    glob_to_regexp(Rest, [C,$\\|Acc]);
glob_to_regexp([$?|Rest], Acc) ->
    glob_to_regexp(Rest, [$.|Acc]);
glob_to_regexp([$*|Rest], Acc) ->
    glob_to_regexp(Rest, [$*,$.|Acc]);
glob_to_regexp([C|Rest], Acc) when C == $\\
                                   orelse C == $\$
                                   orelse C == $^
                                   orelse C == $*
                                   orelse C == $+
                                   orelse C == $?
                                   orelse C == $.
                                   orelse C == $(
                                   orelse C == $)
                                   orelse C == $|
                                   orelse C == ${
                                   orelse C == $}
                                   orelse C == $[
                                   orelse C == $] ->
    glob_to_regexp(Rest, [C, $\\|Acc]);
glob_to_regexp([C|Rest], Acc) ->
    glob_to_regexp(Rest, [C|Acc]).

%% @doc Конвертирует строку в ее шестнадцетиричное представление.
%% @end
-spec hexstr(string()) -> binary().
hexstr(Str) when is_list(Str) ->
    hexstr(list_to_binary(Str));
hexstr(Bin) ->
    << <<(hex(A)), (hex(B))>> || <<A:4,B:4>> <= Bin >>.

hex(X) ->
    element(X+1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f}).
