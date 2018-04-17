%%%-------------------------------------------------------------------
%%% @author vttek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2018 12:23 PM
%%%-------------------------------------------------------------------
-module(nodeB).
-author("vttek").

%% API
-include("header.hrl").
%% API
-export([start/1, send_msg_heart/1, create_msg_heart/0, rpc_send_msg_heart/0]).

-spec get_error_status(error_status()) -> 1|0.
get_error_status(Flag) when Flag == 0 -> 1;
get_error_status(Flag) -> 0.

send_msg_heart(MsgHeart) ->
  io:format("Sending msg from node ~p~n", [node()]),
  nodeB ! {self(), MsgHeart}.
start(NodeA) -> register(nodeB, spawn_link(fun() -> loop(NodeA, 0) end)).
rpc_send_msg_heart() ->
  rpc:call(nodeA@client, nodeA, send_msg_heart, [nodeB:create_msg_heart()]).
create_msg_heart() ->
  CheckEth0 = common:check_eth0(list_to_atom("nodeB@VTK-USER", nodeB)),
  CurrentStatus = 1,
  case CheckEth0 of
    true -> CurrentStatus = 0;
    false -> CurrentStatus = 1
  end,
  #msg_heartbeat{
    node_id = nodeB,
    current_status = 1
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
loop(NodeA, StatusError) ->
  timer:sleep(1000),
  rpc:call(NodeA, nodeA, send_msg_heart, [nodeB:create_msg_heart()]),
  receive
    {From, _MsgHeart} ->
      io:format("NodeB received msg from ~p~n", [From]),
      rpc:call(NodeA, nodeA, send_msg_heart, [nodeB:create_msg_heart()]),
      loop(NodeA, StatusError)
  after 1000 ->
    send_error_server(common:get_ip_node(list_to_atom("nodeB@VTK-USER")), common:get_ip_node(nodeB), get_error_status(StatusError)),
    loop(NodeA, get_error_status(StatusError))
  end.
