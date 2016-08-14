%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc Клиент к RabbitMQ
%%%
%%% @end
%%% Created : 30 Apr 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(sutil_mq_consumer).

-behaviour(plain_fsm).

-export([start_link/6, start_link/7, ack/0, reject/0]).

%% plain_fsm exports
-export([data_vsn/0, code_change/3]).

-include_lib("plain_fsm/include/plain_fsm.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-include("../include/log.hrl").

-define(JSON_CONTENT_TYPE, <<"application/json">>).
-define(UTF_CONTENT_ENCODING, <<"UTF-8">>).

-type rabbit() :: atom().
-type ack() :: true | ack.
-type reject() :: false | reject.

-type handler() :: fun ((rabbit(), MsgType::string(), Msg::binary(),
                         ContentType::string(), ContentEncoding::string(),
                         Headers::[{term(), term(), binary()}]
                                  ) -> ack() | reject()).
-type exchange_type() :: string().

-record(state, {rabbit :: atom(),
                channel :: atom(),
                channel_pid :: pid(),
                handler :: handler(),
                exchange :: string(),
                exchange_type :: exchange_type(),
                exchange_opts = [] :: proplists:proplist(),
                queue :: string(),
                queue_opts = [] :: proplists:proplist(),
                queue_tag :: term(),
                key :: string(),
                qos :: infinity | integer()
               }).

ack() ->
    ack.

reject() ->
    reject.

-spec start_link(Rabbit::atom(), Channel::atom(),
                 Exchange::{string(), exchange_type(), proplists:proplist()},
                 Queue::{string(), proplists:proplist()},
                 Key::string(),
                 Hanlder::handler())
                -> {ok, pid()}.
start_link(Rabbit, Channel, Exchange, Queue, Key, Handler) ->
    start_link(Rabbit, Channel, Exchange, Queue, Key, Handler, infinity).

-spec start_link(Rabbit::atom(), Channel::atom(),
                 Exchange::{string(), exchange_type(), proplists:proplist()},
                 Queue::{string(), proplists:proplist()},
                 Key::string(),
                 Handler::handler(),
                 HandlerPrefetch::infinity | integer())
                -> {ok, pid()}.
start_link(Rabbit, Channel, Exchange, Queue, Key, Handler, HandlerPrefetch) ->

    {ExName, ExType, ExOpts} = Exchange,
    {QueName, QueOpts} = Queue,
    Pid = plain_fsm:spawn_link(?MODULE,
                               fun () ->
                                       init(#state{rabbit=Rabbit,
                                                   channel=Channel,
                                                   handler=Handler,
                                                   exchange=ExName,
                                                   exchange_type=ExType,
                                                   exchange_opts=ExOpts,
                                                   queue=QueName,
                                                   queue_opts=QueOpts,
                                                   key=Key
                                                   qos=HandlerPrefetch})
                               end),
    {ok, Pid}.

init(S=#state{channel_pid=undefined,
              channel=Channel,
              rabbit=Rabbit,
              exchange=Exchange,
              exchange_type=ExType,
              exchange_opts=ExOpts,
              queue=Queue,
              queue_opts=QueueOpts,
              key=Key,
              qos=HandlerPrefetch
             }) ->
    usagi_agent:wait_rabbit(Rabbit, infinity),
    {ok, ChPid} = usagi_agent:get_channel(Rabbit, Channel),
    usagi:start_exchange(Channel, Exchange, ExType, ExOpts),
    usagi:start_queue(Channel, Queue, QueueOpts),
    usagi:bind_queue(Channel, Exchange, Queue, Key),
    case HandlerPrefetch of
        infinity -> ok;
        Prefetch ->
            usagi_channel:qos(Channel, Prefetch)
    end,
    {ok, QT} = usagi:consume_queue(Channel, Queue, self()),
    erlang:monitor(process, ChPid),
    loop(S#state{channel_pid=ChPid,
                 queue_tag=QT
                }).

loop(S=#state{rabbit=R,
              channel_pid=ChPid,
              queue=Q,
              queue_tag=QT,
              handler=Hlr
             }) ->
    Parent = plain_fsm:info(parent),
    receive
        {'DOWN', _, _, ChPid, _Reason} ->
            init(S#state{channel_pid=undefined});
        {'EXIT', Parent, Reason} ->
            usagi:cancel_consume(ChPid, QT),
            plain_fsm:parent_EXIT(Reason, S);
        {#'basic.deliver'{delivery_tag = Tag},
         #amqp_msg{props=#'P_basic'{content_type=CT, content_encoding=CE, headers=Hs},
                   payload=Msg}} ->
            case handle_mq_message(R, Hlr, Msg, CT, CE, Hs) of
                {error, Error} ->
                    usagi:reject(ChPid, Tag),
                    ?error("rabbit ~p, queue ~p, handler ~p ~p", [R, Q, Hlr, Error]);
                {ok, Ack} when Ack == ok;
                               Ack == true;
                               Ack == ack ->
                    usagi:ack(ChPid, Tag);
                {ok, Reject} when Reject == false;
                                  Reject == reject ->
                    usagi:reject(ChPid, Tag)
            end,
            loop(S);
        Msg ->
            plain_fsm:handle_msg(Msg, S, fun(S1) -> loop(S1) end)
    end.

handle_mq_message(Rabbit, Hlr, Msg, ContentType, ContentEncoding, Headers) ->
    sutil:maybe(fun () ->
                        MsgType =
                            case lists:keysearch(sutil_mq:message_type_header(),
                                                 1,
                                                 sutil:from_maybe(Headers, [])) of
                                false ->
                                    undefined;
                                {value, {_, _, Type}} ->
                                    Type
                            end,
                        Hlr(Rabbit, MsgType, Msg, ContentType, ContentEncoding, Headers)
                end,
                fun (Ok) -> Ok end,
                fun (Error) -> {error, Error} end).

code_change(_OldVsn, State, _Extra) ->
    {ok, {State, data_vsn()}}.

data_vsn() ->
    1.
