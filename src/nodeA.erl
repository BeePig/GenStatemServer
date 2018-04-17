%%%-------------------------------------------------------------------
%%% @author vttek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2018 12:23 PM
%%%-------------------------------------------------------------------
-module(nodeA).
-author("vttek").

%% API
-include("header.hrl").
%% API
-export([start/1, send_msg_heart/1, create_msg_heart/0, send_msg/0]).

-spec get_error_status(error_status()) -> 1|0.
get_error_status(Flag) when Flag == 0 -> 1;
get_error_status(Flag) -> 0.

send_msg_heart(MsgHeart) ->
  io:format("Sending msg from node ~p~n", [node()]),
  nodeA ! {self(), MsgHeart}.

send_msg() -> true.
start(NodeB) -> register(nodeA, spawn_link(fun() -> loop(NodeB, 0) end)).

create_msg_heart() ->
  CheckEth0 = common:check_eth0(list_to_atom("nodeA@VTK-USER", nodeA)),
  CurrentStatus = 1,
  case CheckEth0 of
    true -> CurrentStatus = 0;
    false -> CurrentStatus = 1
  end,
  #msg_heartbeat{
    node_id = nodeA,
    current_status = CurrentStatus
  }.
send_error_server(Ipsender, IpTimeout, FlagError) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  MsgError = #msg_error{
    ip_send = Ipsender,
    ip_time_out = IpTimeout,
    error_status = FlagError
  },
  gen_udp:send(Socket, "localhost", ?PORT_UDP_SERVER, term_to_binary(MsgError)),
  io:format("sending error msg to server....."),
  gen_udp:close(Socket).

%%--------------------------------------------------------------------
%% @doc
%% loop sending and receiving message from 2 pair node, default
%% StatusError is 0
%% @spec
%% @end
%%--------------------------------------------------------------------
loop(NodeB, StatusError) ->
  timer:sleep(1000),
  rpc:call(NodeB, nodeB, send_msg_heart, [nodeA:create_msg_heart()]),
  receive
    {From, _MsgHeart} ->
      io:format("NodeA has received msg from ~p~n", [From]),
      rpc:call(NodeB, nodeA, send_msg_heart, [nodeB:create_msg_heart()]),
      loop(NodeB, StatusError)
  after 1000 ->
    IpSent = common:get_ip_node(list_to_atom("nodeA@VTK-USER")),
    io:format("Ip sent: ~p~n", [IpSent]),
    IpTimeout = common:get_ip_node(NodeB),
    io:format("Ip timeout: ~p~n", [IpTimeout]),
    send_error_server(IpSent, IpTimeout, get_error_status(StatusError)),
    loop(NodeB, get_error_status(StatusError))
  end.
