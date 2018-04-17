%%%-------------------------------------------------------------------
%%% @author vttek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2018 3:00 PM
%%%-------------------------------------------------------------------
-author("vttek").
-define(IP_NODE_A, "192.168.1.10").
-define(IP_NODE_B, "192.168.1.11").
-define(IP_UDP_SERVER, "192.168.1.12").
-define(PORT_UDP_SERVER, 6789).
-type error_status() :: 0|1.
-type nodename() :: nodeA|nodeB.
-type node_ip() :: string | atom.
-type nodes() :: {nodename(),node_ip()}.
-export_type([nodes/0,error_status/0,nodename/0]).
-record(package, {
  cmdCode,
  tlv = #{msisdn => -1, key => "None", name => "None"}
}).
-record(msg_heartbeat, {
  node_id :: string(),
  current_status :: integer()
}).
-record(msg_error, {
  ip_send :: node_ip(),
  ip_time_out :: node_ip(),
  error_status :: error_status()
}).


