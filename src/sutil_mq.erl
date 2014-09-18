%%% @author Vladimir G. Sekissov <>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%% RabbitMQ interface
%%% @end
%%% Created :  1 Aug 2014 by Vladimir G. Sekissov <>

-module(sutil_mq).

-behaviour(plain_fsm).

-export([start_link/2]).

%% plain_fsm exports
-export([data_vsn/0, code_change/3]).

-include_lib("plain_fsm/include/plain_fsm.hrl").

-export_type([mq_spec/0]).

-type mq_spec() :: {type, network}
                 | {vhost, string()}
                 | {host, inet:ip_addr()}
                 | {port, integer()}
                 | {username, string()}
                 | {password, string()}
                 | {ssl_options, none | [term()]}.

-spec start_link(Rabbit::atom(), ServerSpecs::[[mq_spec()]]) -> {ok, pid()}.
start_link(Rabbit, ServerSpecs) ->
    {ok, Rabbits} = application:get_env(usagi, rabbits),
    Rabbits1 = deepprops:set(Rabbit, ServerSpecs, Rabbits),
    application:set_env(usagi, rabbits, Rabbits1),
    application:set_env(amqp_client, prefer_ipv6, false),
    application:stop(usagi),
    ok = application:start(usagi),
    wait_usagi_agent(),
    Pid = plain_fsm:spawn_link(?MODULE,
                               fun () ->
                                       init({})
                               end),
    {ok, Pid}.

init({}) ->
    erlang:process_flag(trap_exit, true),
    loop([]).

loop(State) ->
    Parent = plain_fsm:info(parent),
    receive
        {'EXIT', Parent, Reason} ->
            plain_fsm:parent_EXIT(Reason, State);
        Msg ->
            plain_fsm:handle_msg(Msg, State, fun(S1) -> loop(S1) end)
    end.

wait_usagi_agent() ->
    case whereis(usagi_agent) of
        undefined ->
            receive
            after 100 ->
                    wait_usagi_agent()
            end;
        Pid when is_pid(Pid) ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, {State, data_vsn()}}.

data_vsn() ->
    1.
