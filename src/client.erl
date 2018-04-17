%%%-------------------------------------------------------------------
%%% @author vttek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2018 2:54 PM
%%%-------------------------------------------------------------------
-module(client).
-author("vttek").
-include("header.hrl").
%% API
-export([connect/1, send/5, request_data/2, request_package/5, create_package/4]).

create_package(CmdCode, MsIsdn, Key, Name) ->
  Re = #package{cmdCode = CmdCode, tlv = #{msisdn => MsIsdn, key => Key, name => Name}}.
%%  io:format("create_package : ~p", [Re]).

connect(Port) ->
  {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, []).

send(Socket, CmdCode, MsIsdn, Key, Name) ->
  Package = create_package(CmdCode, MsIsdn, Key, Name),
  Bin = term_to_binary(Package),
  gen_tcp:send(Socket, Bin).

send(Socket, Data) ->
  gen_tcp:send(Socket, Data).

request_data(Port, Data) ->
  {ok, Socket} = connect(Port),
  send(Socket, Data).

request_package(Port, CmdCode, MsIsdn, Key, Name) ->
  {ok, Socket} = connect(Port),
  send(Socket, CmdCode, MsIsdn, Key, Name).