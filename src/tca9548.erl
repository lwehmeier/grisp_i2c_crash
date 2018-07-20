%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jul 2018 11:59
%%%-------------------------------------------------------------------
-module(tca9548).
-author("Leon Wehmeier").


-define(I2C_M_RD, 16#0001).
% Callbacks
-behavior(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).


init(_Args) ->
  process_flag(trap_exit, true),
  {ok, #{init => true, addr=>16#70}}.
handle_call(Call, _From, State) ->
  try execute_call(Call, State)
  catch throw:Reason -> {reply, {error, Reason}, State}
  end.
handle_cast(Request, _State) -> error({unknown_cast, Request}).
handle_info(Info, _State) -> error({unknown_info, Info}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #{slot := Slot}) -> io:format("ina219 termination requested\r\n").

execute_call({enable_channel, Channel}, State)->
  #{addr:=Addr} = State,
  {reply, enable_channel(Addr, Channel), State};
execute_call({set_channel, Channel}, State)->
  #{addr:=Addr} = State,
  {reply, set_channel(Addr, Channel), State};
execute_call({disable_channel, Channel}, State)->
  #{addr:=Addr} = State,
  {reply, disable_channel(Addr, Channel), State}.

enable_channel(Addr, Ch) ->
  ChBit = round(math:pow(2, Ch)),
  <<Active:8>> = read_channel(Addr),
  New = Active bor ChBit,
  grisp_i2c:msgs([Addr, {write, <<New:8>>}]).
set_channel(Addr, Ch) ->
  ChBit = round(math:pow(2, Ch)),
  grisp_i2c:msgs([Addr, {write, <<ChBit:8>>}]).
disable_channel(Addr, Ch) ->
  ChBit = round(math:pow(2, Ch)),
  <<Active:8>> = read_channel(Addr),
  New = Active band bnot ChBit,
  grisp_i2c:msgs([Addr, {write, <<New:8>>}]).
read_channel(Addr) ->
  grisp_i2c:msgs([Addr, {read, 1, ?I2C_M_RD}]).
