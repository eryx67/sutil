%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Манипуляции с датами и временем
%%%
%%% @end
%%% Created : 25 Feb 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(sutil_date).

-export([local_tz/0, validate_tz/1, local_to_utc/2, utc_to_local/2]).
-export([now_sub_sec/2, now_add_sec/2, now_to_timestamp/1, timestamp_to_now/1]).
-export([day_diff/2, add_days/2, add_seconds/2, next_month/1, end_of_month/1]).
-export([begin_of_month/1, prev_month/1, trunc_seconds/2, day_start/1, day_end/1]).
-export([month_diff/2]).

-type tz() :: string().

-spec local_to_utc(Date::calendar:datetime(), TZ::string()) -> calendar:datetime().
local_to_utc(Date, TZ) ->
    localtime:local_to_utc(Date, sutil:ensure_list(TZ)).

-spec utc_to_local(Date::calendar:datetime(), TZ::string()) -> calendar:datetime().
utc_to_local(Date, TZ) ->
    localtime:utc_to_local(Date, sutil:ensure_list(TZ)).

%% @doc Текущая временная зона
%% @end
-spec local_tz() -> string().
local_tz() ->
    string:strip(os:cmd("date +%Z"), both, $\n).

%% @doc Проверяет временную зону на существование
%% @throws {error, {invalid_tz, tz()}}
%% @end
-spec validate_tz(Zone::tz()) -> tz() | no_return().
validate_tz(B) when is_binary(B) ->
    validate_tz(binary_to_list(B));
validate_tz(TZ) ->
    TZ1 = string:to_upper(TZ),
    case lists:member(TZ1, localtime:list_timezones()) of
        true ->
            TZ1;
        false ->
            throw({error, {invalid_tz, TZ}})
    end.

now_to_timestamp(Time={_Mega, _Sec, Micro}) ->
    {D, {HH, MM, SS}} = calendar:now_to_universal_time(Time),
    {D, {HH, MM, SS + (Micro div 1000) / 1000}}.

timestamp_to_now({D, {HH, MM, SSMS}}) ->
    SS = erlang:trunc(SSMS),
    Seconds = calendar:datetime_to_gregorian_seconds({D, {HH, MM, SS}}) - 62167219200,
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    {Seconds div 1000000, Seconds rem 1000000, erlang:trunc((SSMS - SS) * 1000000)}.

now_sub_sec(Now, Seconds) when Seconds >= 0 ->
    {Mega, Sec, Micro} = Now,
    SubMega = Seconds div 1000000,
    SubSec = Seconds rem 1000000,
    Mega1 = Mega - SubMega,
    Sec1 = Sec - SubSec,
    {Mega2, Sec2} = if Mega1 < 0 ->
                            exit(badarg);
                       (Sec1 < 0) ->
                            {Mega1 - 1, 1000000 + Sec1};
                       true ->
                            {Mega1, Sec1}
                    end,
    if (Mega2 < 0) ->
            exit(badarg);
       true ->
            {Mega2, Sec2, Micro}
    end.

now_add_sec(Now, Seconds) when Seconds >= 0 ->
    {Mega, Sec, Micro} = Now,
    SubMega = Seconds div 1000000,
    SubSec = Seconds rem 1000000,
    Mega1 = Mega + SubMega,
    Sec1 = Sec + SubSec,
    {Mega2, Sec2} = if Mega1 < 0 ->
                            exit(badarg);
                       (Sec1 > 1000000) ->
                            {Mega1 + 1, Sec1 - 1000000};
                       true ->
                            {Mega1, Sec1}
                    end,
    if (Mega2 < 0) ->
            exit(badarg);
       true ->
            {Mega2, Sec2, Micro}
    end.

%% @doc Последний день месяца. Время то же.
%% @end
-spec end_of_month(calendar:datetime()) -> calendar:datetime().
end_of_month({{Y, M, _D}, T}) -> {{Y, M, calendar:last_day_of_the_month(Y, M)}, T}.

%% @doc Первый день месяца. Время то же.
%% @end
-spec begin_of_month(calendar:datetime()) -> calendar:datetime().
begin_of_month({{Y, M, _D}, T}) -> {{Y, M, 1}, T}.

%% @doc Первое возможное время следующего месяца с таким же или меньшим днем.
%% @end
-spec next_month(calendar:datetime()) -> calendar:datetime().
next_month({{Y, M, D}, T}) ->
    {NY, NM} = if M == 12 -> {Y + 1, 1};
                  true -> {Y, M + 1}
               end,
    {find_valid_date({NY, NM, D}), T}.

%% @doc Первое возможное время предыдущего месяца с таким же или меньшим днем.
%% @end
-spec prev_month(calendar:datetime()) -> calendar:datetime().
prev_month({{Y, M, D}, T}) ->
    {PY, PM} = if M == 1 -> {Y - 1, 12};
                  true -> {Y, M - 1}
               end,

    {find_valid_date({PY, PM, D}), T}.

find_valid_date(Date={Y, M, D}) when M >= 1,
                                     M =< 12,
                                     Y > 0,
                                     D > 0 ->
    case calendar:valid_date(Date) of
        true ->
            Date;
        false ->
            find_valid_date({Y, M, D - 1})
    end.

month_diff(To, From) ->
    month_diff(To, From, 0).

month_diff(To={{ToY, _, _}, _}, _From={{FromY, FM, FD}, FT}, Acc) when ToY > FromY ->
    month_diff(To, {{ToY, FM, FD}, FT}, Acc + (ToY - FromY) * 12);
month_diff(_To={{FY, ToM, _}, _}, _From={{FY, FromM, _}, _}, Acc) ->
    Acc + ToM - FromM.

%% @doc Разница в днях, `Days1 - Days2`
%% @end
day_diff({D1, _}, D2) ->
    day_diff(D1, D2);
day_diff(D1, {D2, _}) ->
    day_diff(D1, D2);
day_diff(D1, D2) ->
    Days1 = calendar:date_to_gregorian_days(D1),
    Days2 = calendar:date_to_gregorian_days(D2),
    Days1 - Days2.

%% @doc Добавляет дни к времени. Время остается тем же.
%% @end
add_days({Date, Time}, N) ->
    {add_days(Date, N), Time};
add_days(Date, N) ->
    New = calendar:date_to_gregorian_days(Date) + N,
    calendar:gregorian_days_to_date(New).

%% @doc Добавляет секунды ко времени.
%% @end
add_seconds(Date, N) ->
    New = calendar:datetime_to_gregorian_seconds(Date) + N,
    calendar:gregorian_seconds_to_datetime(New).

%% @doc Обрезает время до кратного _Sec_
%% @end
trunc_seconds(DateTime, Sec) when is_integer(Sec),
                                  Sec > 0 ->
    V = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(V - (V rem Sec)).

%% @doc Время начала дня.
%% @end
day_start({Date, _Time}) ->
    {Date, {0, 0, 0}}.

%% @doc Время конца дня.
%% @end
day_end({Date, _Time}) ->
    {Date, {23, 59, 59}}.
