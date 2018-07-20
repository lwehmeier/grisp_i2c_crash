## Test repo to reproduce grisp crash

compile and nl vl6180x, tca9548, distance\_server, then run
```
Erlang/OTP 21 [erts-10.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.0  (abort with ^G)
(helloworld@grisp_board)1> gen_server:start_link({local, tca9548}, tca9548, [],[]).
{ok,<0.121.0>}
(helloworld@grisp_board)2> gen_server:start_link({local, distance_server}, distance_server, [], []).
init sensor: front_left on channel: 6
init sensor: front_right on channel: 7
init sensor: right_front on channel: 4
init sensor: right_rear on channel: 5
init sensor: left_front on channel: 3

```
The board crashes reliably sometime during the init sequence.

### What the code does

The tca9548 is an i2c multiplexer which is needed to use multiple vl6180x distance sensors as ST considered giving them no address selection pins for their i2c address a good thing..
The vl6180x is a distance sensor with a slightly more complex init sequence, see corresponding driver file.
The distance\_server module sets up multiple vl6180 to generate an interrupt (GPIO) when a threshold is reached.
