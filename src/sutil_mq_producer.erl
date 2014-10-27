%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc
%%% RabbitMQ producer
%% ++++
%% <p/>
%% ++++
%%% @end
%%% Created : 30 Sep 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(sutil_mq_producer).

-behaviour(gen_server).

-export([start_link/6, start_link/7, state/1, publish/2, publish_async/2]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-include("../include/sutil.hrl").
-include("../include/log.hrl").

-define(CONFIRM_TIMEOUT, 60).

-type exchange_type() :: string().
-type handler() :: fun((term()) -> [{term(), proplists:proplist(), proplists:proplist()}]).

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
                amq_status = disconnected :: connected | disconnected,
                amq_watchdog :: pid()
               }).

-spec start_link(Rabbit::atom(), Channel::atom(),
                 Exchange::{string(), exchange_type(), proplists:proplist()},
                 Queue::{string(), proplists:proplist()},
                 Key::string(),
                 handler())
                -> {ok, pid()}.
start_link(Rabbit, Channel, Exchange, Queue, Key, Handler) ->
    gen_server:start_link(?MODULE, {Rabbit, Channel, Exchange, Queue, Key, Handler}, []).

-spec start_link(Name::atom(), Rabbit::atom(), Channel::atom(),
                 Exchange::{string(), exchange_type(), proplists:proplist()},
                 Queue::{string(), proplists:proplist()},
                 Key::string(),
                 handler())
                -> {ok, pid()}.
start_link(Name, Rabbit, Channel, Exchange, Queue, Key, Handler) ->
    gen_server:start_link({local, Name},
                          ?MODULE, {Rabbit, Channel, Exchange, Queue, Key, Handler}, []).

state(Pid) ->
    gen_server:call(Pid, {state}).

publish(Pid, Data) ->
    gen_server:call(Pid, {publish, Data}, infinity).

publish_async(Pid, Data) ->
    gen_server:cast(Pid, {publish, Data}).

%% gen_server callbacks
init({Rabbit, Channel, Exchange, Queue, Key, Handler}) ->
    {ExName, ExType, ExOpts} = Exchange,
    {QueName, QueOpts} = Queue,
    State1 = #state{channel_pid=undefined,
                    channel=Channel,
                    rabbit=Rabbit,
                    exchange=ExName,
                    exchange_type=ExType,
                    exchange_opts=ExOpts,
                    queue=QueName,
                    queue_opts=QueOpts,
                    key=Key,
                    handler=Handler
                   },
    State2 = spawn_amq_watchdog(State1),
    {ok, State2}.

handle_call({state}, _From, S=#state{exchange=Ex,
                                     queue=Queue,
                                     key=Key,
                                     amq_status=AmqStatus}) ->
    Reply = [{exchange, Ex},
             {queue, Queue},
             {key, Key},
             {amq_status, AmqStatus}
            ],
    {reply, Reply, S};
handle_call({publish, _Recs}, _From, S=#state{amq_status=disconnected}) ->
    {reply, {error, disconnected}, S};
handle_call({publish, Recs}, _From, State) ->
    {Ret, State1} = publish_data(Recs, State),
    {reply, Ret, State1}.

handle_cast({publish, _Recs}, S=#state{amq_status=disconnected}) ->
    {noreply, S};
handle_cast({publish, Recs}, State) ->
    {_, State1} = publish_data(Recs, State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({amq_status, Status}, State) ->
    {noreply, State#state{amq_status=Status}};
handle_info({'DOWN', _, process, Pid, normal}, S=#state{amq_watchdog=Pid}) ->
    {noreply, S#state{amq_watchdog=undefined}};
handle_info({'DOWN', _, process, Pid, Reason}, S=#state{amq_watchdog=Pid}) ->
    ?debug("amq watchdog exited with error ~p", [Reason]),
    {noreply, spawn_amq_watchdog(S#state{amq_status=disconnected})};
handle_info(_Msg, S=#state{amq_status=disconnected}) ->
    {noreply, S};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

publish_data(Recs, S=#state{handler=Hlr}) ->
    amq_publish_chunk(lists:flatten(Hlr(Recs)), S).

amq_publish_chunk(Datas, S=#state{rabbit=Rabbit,
                                  channel=ChannelName,
                                  exchange=Exchange,
                                  key=Key
                                 }) ->
    case sutil:maybe(
           fun () ->
                   {ok, ChannelPid} = usagi_agent:get_channel(Rabbit, ChannelName),
                   amqp_channel:wait_for_confirms(ChannelPid, 0),
                   lists:foreach(fun ({Data, Opts, Props}) ->
                                         ok = usagi:publish(ChannelPid, Exchange, Key,
                                                            Data, Opts, Props)
                                 end,
                                 Datas),
                   amqp_channel:wait_for_confirms_or_die(ChannelPid, ?CONFIRM_TIMEOUT)
           end,
           fun (_) -> S end,
           fun (Error) ->
                   ?error("publishing ~p", [Error]),
                   {error, Error, spawn_amq_watchdog(S)}
           end) of
        {ok, S1=#state{}} ->
            {ok, S1};
        {ok, {error, Error, S1}} ->
            {{error, Error}, S1}
    end.

spawn_amq_watchdog(S=#state{rabbit=Rabbit,
                            exchange=ExName,
                            exchange_type=ExType,
                            exchange_opts=ExOpts,
                            channel=Channel,
                            queue=QueName,
                            queue_opts=QueOpts,
                            key=Key}) ->
    Self = self(),
    Pid = spawn(fun () ->
                        amq_watchdog(Self, Rabbit, Channel,
                                     ExName, ExType, ExOpts,
                                     QueName, QueOpts,
                                     Key)
                end),
    erlang:monitor(process, Pid),
    S#state{amq_watchdog=Pid,
            amq_status=disconnected}.

amq_watchdog(Parent, Rabbit, Channel, ExName, ExType, ExOpts, QueName, QueOpts, Key) ->
    usagi_agent:wait_rabbit(Rabbit, infinity),
    {ok, ChannelPid} = usagi_agent:get_channel(Rabbit, Channel),
    amqp_channel:call(ChannelPid, #'confirm.select'{}),
    usagi:start_exchange(ChannelPid, ExName, ExType, ExOpts),
    usagi:start_queue(ChannelPid, QueName, QueOpts),
    usagi:bind_queue(ChannelPid, ExName, QueName, Key),
    Parent ! {amq_status, connected}.
