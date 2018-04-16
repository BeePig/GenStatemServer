%%%-------------------------------------------------------------------
%%% @author vttek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2018 4:01 PM
%%%-------------------------------------------------------------------
-module(gen_statem_server).
-author("vttek").
-include("package.hrl").
-behaviour(gen_statem).
-define(CALLBACK_STATE_FUNCTION_MODE, state_functions).
-define(CALLBACK_HANDLE_FUNCTION_MODE, handle_event_function).


%% API
-export([start_link/1, solve_cmd/2, check_format_MsIsdn/1, listen/1]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0,
  idle/3,
  ready/3,
  select/3,
  start_init_state/0
]).
-define(SERVER, ?MODULE).
-define(READY_STATE, ready).
-define(SELECT_STATE, select).
-define(INIT_STATE, idle).
-define(CLOSED_STATE, closed).
-define(AUTHEN_CMD, "AUTHEN").
-define(INSERT_CMD, "INSERT").
-define(COMMIT_CMD, "COMMIT").
-define(SELECT_CMD, "SELECT").
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(MODULE_NAME, gen_statem_server).
-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
  io:format("Start link~n", []),
  _Package = #package{},
  gen_statem:start_link({local, ?SERVER}, ?MODULE, Args, []),
  start_init_state().

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------

start_init_state() ->
  gen_statem:cast(?MODULE, idle).
start_ready_state() ->
  gen_statem:cast(?MODULE, ready).
start_select_state() ->
  gen_statem:cast(?MODULE, select).

init(Args) ->
  io:format("start init ~p ~n", [Args]),
%%  gen_statem:cast(?MODULE_NAME, idle),
  {ok, ?INIT_STATE, Args}.

idle(cast, _, Package) ->
  io:format("idle ~n", []),
  case check_format_MsIsdn(Package) of
    {ok, continue} ->
      case Package#package.cmdCode of
        ?AUTHEN_CMD ->
          {next_state, ?READY_STATE, Package}
      end;
    {error} ->
      closed()
  end,
  {next_state, ?INIT_STATE, Package};

idle(call, _, Package) ->
  {next_state, ?INIT_STATE, Package}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
%%  ?CALLBACK_STATE_FUNCTION_MODE.
  ?CALLBACK_HANDLE_FUNCTION_MODE.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
state_name(_EventType, _EventContent, State) ->
%%  if is_map(State) ->
%%    {next_state, maps:get(state, State), maps:get(package, State)}
%%  end.
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
%%handle_event(_EventType, _EventContent, _StateName, State) ->
%%  NextStateName = the_next_state_name,
%%  {next_state, NextStateName, State}.


handle_event(cast, _, State, Package) ->
  case State of
    ?INIT_STATE ->
      io:format("idle ~n", []),
      case check_format_MsIsdn(Package) of
        {ok, continue} ->
          case Package#package.cmdCode of
            ?AUTHEN_CMD ->
              start_ready_state(),
              {next_state, ?READY_STATE, Package}
          end;
        {error} ->
          closed()
      end;
    ?READY_STATE ->
      io:format("ready ~n", []),
      case check_format_MsIsdn(Package) of
        {ok, continue} ->
          solve_cmd(?READY_STATE, Package)
      end,
      gen_tcp:send(closed_package);
    ?SELECT_STATE ->
      io:format("select ~n", []),
      case check_format_MsIsdn(Package) of
        {ok, continue} ->
          solve_cmd(?SELECT_STATE, Package)
      end,
      gen_tcp:send(closed_package);
    ?CLOSED_STATE ->
      {"ERROR", #{resultCode => "NA"}}
  end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert format of MsIsdn
%%--------------------------------------------------------------------




closed() ->
  {"ERROR", #{resultCode => "NA"}}.
check_format_MsIsdn(Package) when is_record(Package, package) ->
  io:format("check package: ~w~n", [Package]),
  Misidn = maps:get(msisdn, Package#package.tlv),
  io:format("Msisdn: ~p", [Misidn]),
  A = string:str(integer_to_list(Misidn), "198"),
  io:format("Msisdn: ~p", [Misidn]),
  io:format("A ~p", [A]),
  if
    A == 1 ->
      {ok, continue};
    true ->
      {error}
  end.
%%{ok, Socket} = gen_tcp:connect({127,0,0,1},1111,[]).
%%gen_tcp:send(Socket,gen_statem_server:create_package("AUTHEN",1987654321,vttek,username)).
solve_cmd(CurrentState, Package) ->
  io:format("Current state is: ~w~n", [CurrentState]),
  case Package#package.cmdCode of
    ?AUTHEN_CMD ->
      Value = maps:get(key, Package#package.tlv),
      io:format("Value of key ~p~n", [Value]),
      if
        Value =:= "vttek" ->
          {#{msisdn => maps:get(msisdn, Package#package.tlv), resultCode => "OK", statusSubcriber => "OK"}, closedPackage};
        true ->
          ok
      end,
      #{msisdn => maps:get(msisdn, Package#package.tlv), resultCode => "NOK"},
      closedPackage,
      gen_statem:call(?MODULE, #{state => ?READY_STATE, package =>Package});
    ?SELECT_CMD ->
      if CurrentState =/= ?SELECT_STATE ->
        {#{msisdn => maps:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage};
        true ->
          ok
      end,
      #{msisdn => maps:get(msisdn, Package#package.tlv), resultCode => "OK", name => username},
      closedPackage;
    ?INSERT_CMD ->
      if
        CurrentState =/= ?READY_STATE ->
          {#{msisdn => maps:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage};
        true ->
          ok
      end,
      #{msisdn => maps:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage,
      insert_subcriber(Package);
    ?COMMIT_CMD ->
      if
        CurrentState =/= ?READY_STATE ->
          {#{msisdn => maps:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage};
        true ->
          ok
      end,
      %% send to select state
      gen_statem:cast(?MODULE, select)
  end.

ready(cast, _, {CurrentState, Package}) ->
  io:format("ready ~n", []),
  case check_format_MsIsdn(Package) of
    {ok, continue} ->
      solve_cmd(CurrentState, Package)
  end,
  gen_tcp:send(closed_package),
  {next_State, ?SELECT_STATE, Package}.

select(cast, _, {CurrentState, Package}) ->
  io:format("select ~n", []),
  case check_format_MsIsdn(Package) of
    {ok, continue} ->
      solve_cmd(CurrentState, Package)
  end,
  gen_tcp:send(closed_package).
listen(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  accept(LSocket).

accept(LSocket) ->
  io:format("accept~n", []),
  {ok, Socket} = gen_tcp:accept(LSocket),
  io:format("Socket ~p~n", [Socket]),
  loop(Socket).
%%  accept(LSocket).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      io:format("Have receive data: ~w~n", [Bin]),
      Package = binary_to_term(Bin),
      start_link(Package);
%%      gen_statem:call(?MODULE, #{state => ?INIT_STATE, package =>Package});
    {error, closed} ->
      ok
  end.

insert_subcriber(Package) ->
  "insert data into system".
