%%%-------------------------------------------------------------------
%%% @author Leon Wehmeier
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2018 14:05
%%%-------------------------------------------------------------------
-module(distance_server).
-author("Leon Wehmeier").

% Callbacks
-behavior(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
%% debug
-export([read_sensor/1, startSensor/1]).


init(_Args) ->
  process_flag(trap_exit, true),
  startSensor(front_left),
  startSensor(front_right),
  startSensor(right_front),
  startSensor(right_rear),
  startSensor(left_front),
  startSensor(left_rear),
  {ok, #{init => true}}.
handle_call(Call, _From, State) ->
  try execute_call(Call, State)
  catch throw:Reason -> {reply, {error, Reason}, State}
  end.
handle_cast(Request, _State) -> error({unknown_cast, Request}).
handle_info(Info, _State) -> error({unknown_info, Info}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _Arg) -> io:format("distance server termination requested\r\n").

execute_call({read_distance, Sensor}, State)->
  {reply, read_sensor(Sensor), State};
execute_call({updateInterruptDistance, Distance}, State)->
  setDistance(front_left, Distance),
  setDistance(front_right, Distance),
  setDistance(right_front, Distance),
  setDistance(right_rear, Distance),
  setDistance(left_front, Distance),
  setDistance(left_rear, Distance),
  {reply, ok, State}.
startSensor(Sensor)->
  timer:sleep(25),
  gen_server:call(tca9548, {set_channel, get_sensor_pin(Sensor)}),
  timer:sleep(25),
  vl6180x:initVL(),
  io:format("init sensor: ~p on channel: ~p ~n", [Sensor, get_sensor_pin(Sensor)]),
  timer:sleep(50), %wait for sensor to initialise. Doesn't work reliably without this delay..
  vl6180x:startContinous(),
  timer:sleep(5),
  vl6180x:setInterruptThreshold(110),
  vl6180x:readRange(16#29),
  vl6180x:enableGPIO(),
  vl6180x:setScaling(2).
read_sensor(Sensor)->
  gen_server:call(tca9548, {set_channel, get_sensor_pin(Sensor)}),
  vl6180x:readRange(16#29).
setDistance(Sensor, Value)->
  gen_server:call(tca9548, {set_channel, get_sensor_pin(Sensor)}),
  timer:sleep(50),
  vl6180x:setInterruptThreshold(Value),
  timer:sleep(50).
get_sensor_pin(front_left) -> 6;
get_sensor_pin(front_right) -> 7;
get_sensor_pin(left_front) ->3;
get_sensor_pin(left_rear) -> 2;
get_sensor_pin(right_front) -> 4;
get_sensor_pin(right_rear) -> 5.
