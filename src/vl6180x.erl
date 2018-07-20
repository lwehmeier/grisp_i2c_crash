%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2018 14:21
%%%-------------------------------------------------------------------
-module(vl6180x).
-author("Leon Wehmeier").
-define(SYSTEM__FRESH_OUT_OF_RESET, 16#16).
-define(SYSRANGE__START, 16#18).
-define(RESULT__INTERRUPT_STATUS_GPIO, 16#4f).
-define(RESULT__RANGE_VAL, 16#62).
-define(SYSRANGE__THRESH_LOW, 16#1A).
-define(SYSRANGE__THRESH_HIGH, 16#19).
-define(SYSTEM__INTERRUPT_CLEAR, 16#15).
-define(SYSRANGE__MAX_CONVERGENCE_TIME, 16#1C).
-define(INTERLEAVED_MODE__ENABLE , 16#2a3).
-define(READOUT__AVERAGING_SAMPLE_PERIOD , 16#10a).
-define(SYSALS__ANALOGUE_GAIN , 16#3f).
-define(SYSRANGE__VHV_REPEAT_RATE , 16#31).
-define(SYSALS__INTEGRATION_PERIOD , 16#40).
-define(SYSRANGE__VHV_RECALIBRATE , 16#2e).
-define(SYSRANGE__INTERMEASUREMENT_PERIOD , 16#1B).
-define(SYSALS__INTERMEASUREMENT_PERIOD , 16#3e).
-define(SYSTEM__INTERRUPT_CONFIG_GPIO , 16#14).
-define(SYSTEM_MODE_GPIO1 , 16#11).
-define(RANGE_SCALER, 16#96). %magic, undocumented register
-define(I2C_SLAVE__DEVICE_ADDRESS , 16#212).
-define(I2C_M_RD, 16#0001).

% Callbacks
-behavior(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
%% API
-export([setScaling/1, initVL/0, writeReg8/2, writeReg16/2, readReg8/1, readReg16/1, startRead/0, readRange/1, startContinous/0, setAddr/1, enableGPIO/0, setInterruptThreshold/1]).


start_link() -> gen_server:start_link(?MODULE, [], []).
init(I2CAddr) ->
  process_flag(trap_exit, true),
  initVL(),
  startContinous(),
  setAddr(I2CAddr),
  {ok, #{init => true, addr=>I2CAddr}}.
handle_call(Call, _From, State) ->
  try execute_call(Call, State)
  catch throw:Reason -> {reply, {error, Reason}, State}
  end.
handle_cast(Request, _State) -> error({unknown_cast, Request}).
handle_info(Info, _State) -> error({unknown_info, Info}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #{slot := Slot}) -> io:format("ina219 termination requested\r\n").

execute_call(get_distance, State)->
  #{addr:=Addr} = State,
  {reply, readRange(Addr), State}.



writeReg16(Reg, Data) ->
  grisp_i2c:msgs([16#29, {write, <<Reg:16, Data:16>>}]).
writeReg8(Reg, Data) ->
  grisp_i2c:msgs([16#29, {write, <<Reg:16, Data:8>>}]).
writeReg8_Addr(Addr, Reg, Data) ->
  grisp_i2c:msgs([Addr, {write, <<Reg:16, Data:8>>}]).
readReg16(Reg) ->
  grisp_i2c:msgs([16#29, {write, <<Reg:16>>}, {read, 2, ?I2C_M_RD}]).
readReg8(Reg) ->
  grisp_i2c:msgs([16#29, {write, <<Reg:16>>}, {read, 1, ?I2C_M_RD}]).
readReg8_Addr(Addr, Reg) ->
  grisp_i2c:msgs([Addr, {write, <<Reg:16>>}, {read, 1, ?I2C_M_RD}]).

initVL()-> %magic sequence adopted from adafruit library
  %app note mandatory
  writeReg8(16#207, 16#01),
  writeReg8(16#208, 16#01),
  writeReg8(16#096, 16#00),
  writeReg8(16#097, 16#FD), % // RANGE_SCALER = 253
  writeReg8(16#0E3, 16#00),
  writeReg8(16#0E4, 16#04),
  writeReg8(16#0E5, 16#02),
  writeReg8(16#0E6, 16#01),
  writeReg8(16#0E7, 16#03),
  writeReg8(16#0F5, 16#02),
  writeReg8(16#0D9, 16#05),
  writeReg8(16#0DB, 16#CE),
  writeReg8(16#0DC, 16#03),
  writeReg8(16#0DD, 16#F8),
  writeReg8(16#09F, 16#00),
  writeReg8(16#0A3, 16#3C),
  writeReg8(16#0B7, 16#00),
  writeReg8(16#0BB, 16#3C),
  writeReg8(16#0B2, 16#09),
  writeReg8(16#0CA, 16#09),
  writeReg8(16#198, 16#01),
  writeReg8(16#1B0, 16#17),
  writeReg8(16#1AD, 16#00),
  writeReg8(16#0FF, 16#05),
  writeReg8(16#100, 16#05),
  writeReg8(16#199, 16#05),
  writeReg8(16#1A6, 16#1B),
  writeReg8(16#1AC, 16#3E),
  writeReg8(16#1A7, 16#1F),
  writeReg8(16#030, 16#00),
  %app note recommended
  writeReg8(16#11, 16#10),
  writeReg8(16#10a, 16#30),
  writeReg8(16#3f, 16#46),
  writeReg8(16#31, 16#FF),
  writeReg8(16#40, 16#63),
  writeReg8(16#2e, 16#1),
  %optional
  writeReg8(16#1b, 16#9),
  writeReg8(16#3e, 16#31),
  writeReg8(16#14, 16#24),

  writeReg8(?SYSRANGE__MAX_CONVERGENCE_TIME, 16#30),
  writeReg8(?INTERLEAVED_MODE__ENABLE, 16#0),

  %init defaults
  writeReg8(?READOUT__AVERAGING_SAMPLE_PERIOD, 16#30),
  writeReg8(?SYSALS__ANALOGUE_GAIN, 16#46),
  writeReg8(?SYSRANGE__VHV_REPEAT_RATE, 16#ff),
  writeReg16(?SYSALS__INTEGRATION_PERIOD, 16#50),
  writeReg8(?SYSRANGE__VHV_RECALIBRATE, 16#1),
  writeReg8(?SYSRANGE__INTERMEASUREMENT_PERIOD, 16#09),
  writeReg8(?SYSALS__INTERMEASUREMENT_PERIOD, 16#31),
  writeReg8(?SYSTEM__INTERRUPT_CONFIG_GPIO, 16#01).

startRead()->
  writeReg8(?SYSRANGE__START, 1),
  readRange_single().
startContinous()->
  writeReg8(?SYSRANGE__INTERMEASUREMENT_PERIOD, 5),%20hz update
  writeReg8(?SYSRANGE__START, 3).

readRange(Addr)->
  <<Range:8/integer-little>> = readReg8_Addr(Addr, ?RESULT__RANGE_VAL),
  writeReg8_Addr(Addr, ?SYSTEM__INTERRUPT_CLEAR, 1),
  trunc(Range).

readRange_single()->
  <<_:5, IRQ:1, _:2>> = readReg8(?RESULT__INTERRUPT_STATUS_GPIO),
  case IRQ of
    1 ->
      Range = readReg8(?RESULT__RANGE_VAL),
      writeReg8(?SYSTEM__INTERRUPT_CLEAR, 1),
      Range;
    0 ->
      timer:sleep(250),
      readRange_single()
  end.

setAddr(NewAddr)->
  writeReg8(?I2C_SLAVE__DEVICE_ADDRESS, NewAddr).

enableGPIO()->
  writeReg8(?SYSTEM_MODE_GPIO1, 16#10). %polarity:active low, interrupt output
setInterruptThreshold(Distance)->
  writeReg8(?SYSRANGE__THRESH_LOW, Distance).
setScaling(1)->
  writeReg16(?RANGE_SCALER,253);
setScaling(2)->
  writeReg16(?RANGE_SCALER,127).