%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%%
%%% @end
%%% Created :  1 Aug 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(sutil_torrent).

-export([integer_id/1, maybe_integer_id/1, binary_id/1]).
-export([hexstr/1,hexstr_to_bin/1, is_hexstr/1]).

-export_type([infohash/0]).

-type infohash() :: pos_integer().

-spec integer_id(list(byte()) | binary()) -> pos_integer().
integer_id(ID) ->
    case maybe_integer_id(ID) of
        {error, Error} ->
            error(Error);
        Ret ->
            Ret
    end.

maybe_integer_id(<<ID:160>>) ->
    ID;
maybe_integer_id(BinID) when is_binary(BinID)
                             ,byte_size(BinID) == 40 ->
    maybe_integer_id(binary_to_list(BinID));
maybe_integer_id(StrID) when is_list(StrID)
                             ,length(StrID) == 40 ->
    case is_hexstr(StrID) of
        true ->
            maybe_integer_id(hexstr_to_bin(StrID));
        false ->
            {error, invalid_id}
    end;
maybe_integer_id(StrID) when is_list(StrID) ->
    maybe_integer_id(list_to_binary(StrID));
maybe_integer_id(ID) when is_integer(ID) ->
    ID;
maybe_integer_id(_) ->
    {error, invalid_id}.

-spec binary_id(binary() | pos_integer()) -> binary().
binary_id(ID) when is_binary(ID),
                   size(ID) == 20 ->
    ID;
binary_id(ID) when is_integer(ID) ->
    <<ID:160>>;
binary_id(ID) when is_list(ID),
                   length(ID) == 20 ->
    list_to_binary(ID).

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

is_hexstr(Bin) when is_binary(Bin) ->
    is_hexstr(binary_to_list(Bin));
is_hexstr(Str) ->
    StrLen = length(Str),
    case length(Str) rem 2 of
        0 when StrLen > 0 ->
            lists:all(fun (C) when $0 =< C, C =< $9 ->
                              true;
                          (C) when $A =< C, C =< $F ->
                              true;
                          (C) when $a =< C, C =< $f ->
                              true;
                          (_) ->
                              false
                      end, Str);
        _ ->
            false
    end.

hexstr(Bin) when is_binary(Bin) ->
    hexstr(binary_to_list(Bin));
hexstr(Str) ->
    list_to_hexstr(Str, []).

list_to_hexstr([], Acc) ->
   lists:flatten(lists:reverse(Acc));
list_to_hexstr([H|T], Acc) ->
    list_to_hexstr(T, [to_hex(H)|Acc]).

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
