%%%-------------------------------------------------------------------
%%% @author vttek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2018 4:45 PM
%%%-------------------------------------------------------------------
-module(common).
-author("vttek").
-include("header.hrl").
%% API
-export([get_ip_node/1, check_eth0/2]).
get_ip_node(Node) ->
  {ok, [{Ip, _, _} | _]} = rpc:call(Node, inet, getif, []),
  Ip.

-spec check_eth0(node(), nodename()) -> true | false.
check_eth0(Node, NameNode) ->
  Ip = get_ip_node(Node),
  case NameNode of
    nodeA ->
      if Ip =/= {192, 168, 1, 10} ->
        false;
        true -> true
      end;
    nodeB ->
      if Ip =/= {192, 168, 1, 11} ->
        false;
        true -> true
      end
  end.