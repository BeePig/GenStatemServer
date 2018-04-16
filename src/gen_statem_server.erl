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

-behaviour(gen_statem).

%% API
-export([start_link/0, create_package/4, solve_cmd/2, solve_state/2, check_format_MsIsdn/2, listen/1]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).
-define(SERVER, ?MODULE).
-define(INIT_STATE, "INIT").
-define(READY_STATE, "READY").
-define(SELECT_STATE, "SELECT").
-define(AUTHEN_CMD, "AUTHEN").
-define(INSERT_CMD, "INSERT").
-define(COMMIT_CMD, "COMMIT").
-define(SELECT_CMD, "SELECT").
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-record(state, {}).
-record(package, {cmdCode = "None", tlv = #{msisdn => 0, key => "None", name => "None"}}).


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
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  {ok, ?INIT_STATE, {?INIT_STATE, []}}.


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
  handle_event_function.

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
  if is_map(State) ->
    solve_state(maps:get(state, State), maps:get(package, State)),
    {next_state, maps:get(state, State), maps:get(package, State)}
  end.

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
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

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



create_package(CmdCode, MsIsdn, Key, Name) ->
  #package{cmdCode = CmdCode, tlv = #{msisdn => MsIsdn, key => Key, name => Name}}.


check_format_MsIsdn(Package, CurrentState) when is_record(Package, package) ->
  A = string:str(integer_to_list(map:get(msisdn, Package#package.tlv)), "098"),
  if
    A == 1 ->
      solve_state(CurrentState, Package)
  end,
  {"ERROR", #{resultCode => "NA"}}.

solve_cmd(CurrentState, Package) ->
  case Package#package.cmdCode of
    ?AUTHEN_CMD ->
      Value = map:get(key, Package#package.tlv),
      if
        Value =:= "vttek" ->
          {#{msisdn => map:get(msisdn, Package#package.tlv), resultCode => "OK", statusSubcriber => "OK"}, closedPackage}
      end,
      #{msisdn => map:get(msisdn, Package#package.tlv), resultCode => "NOK"},
      closedPackage,
      gen_statem:call(?MODULE, #{state => ?READY_STATE, package =>Package});
    ?SELECT_CMD ->
      if CurrentState =/= ?SELECT_STATE ->
        {#{msisdn => map:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage}
      end,
      #{msisdn => map:get(msisdn, Package#package.tlv), resultCode => "OK", name => username},
      closedPackage;
    ?INSERT_CMD ->
      if
        CurrentState =/= ?READY_STATE ->
          {#{msisdn => map:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage}
      end,
      #{msisdn => map:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage,
      insert_subcriber(Package);
    ?COMMIT_CMD ->
      if
        CurrentState =/= ?READY_STATE ->
          {#{msisdn => map:get(msisdn, Package#package.tlv), resultCode => "NOK"}, closedPackage}
      end,
      solve_state(?SELECT_STATE, Package)
  end.
solve_state(CurrentState, Package) ->
  case CurrentState of
    ?INIT_STATE ->
      solve_cmd(CurrentState, Package);
    ?READY_STATE ->
      solve_cmd(CurrentState, Package);
    ?SELECT_STATE ->
      solve_cmd(CurrentState, Package)
  end.


listen(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  accept(LSocket).

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  start_link(),
  accept(LSocket).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Package} ->
      solve_state(?INIT_STATE, Package),
      loop(Socket);
    {error, closed} ->
      ok
  end.

insert_subcriber(Package) ->
  "insert data into system".
