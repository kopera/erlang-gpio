# gpio

An Erlang application for interfacing with GPIOs on Linux systems.

This GPIO application allows for reading and writing to GPIO pins as well as 
monitoring input GPIO pins.

# Setup

You need to add `gpio` as a dependency to your project. If you are using
`rebar3`, you can add the following to your `rebar.config`:

```erlang
{deps, [
    {gpio, "0.6.1"}
]}.
```

Also ensure that `gpio` is added as a dependency to your application, by
updating your `.app.src` file:

```erlang
{application, my_app, [

    {applications, [
        kernel,
        stdlib,

        gpio  % <- You need this in your applications list
    ]}
]}.
```

# Usage

## Opening the gpio chip

The following will open gpiochip2 (typically called GPIO3):

```erlang
> {ok, Chip} = gpio:open_chip("/dev/gpiochip2").
{ok, #Ref<0.3061467712.2848194561.68326>}
```

Once the chip is open you need to either use `gpio:open_lines/5` if you want
to read/write values to/from the lines, or `gpio:open_line_events/5` if you want
to monitor a line.

## Opening gpio lines
The following will open a single line `IO1` from the previously open
chip (typically this would be `GPIO3_IO1`). The opened line is configured as an
output with a default value of `0`. The last parameter is used to communicate
to the kernel a description of the chip use. If `debugfs` is enabled, this
information is visible in `/sys/kernel/debug/gpio`.

```erlang
> {ok, SingleLine} = gpio:open_lines(Chip, [1], [output], [0], "my-application").
{ok, #Ref<0.3061467712.2848194561.68334>}
```

To write a `0` to the GPIO line, you can use:

```erlang
ok = gpio:write_lines(SingleLine, {0}).
```

Alternatively, to write a `1` to the GPIO line, you can use:
```erlang
ok = gpio:write_lines(SingleLine, {1}).
```

The `gpio:write_lines/2` function takes a tuple of `0` and `1` values. The arity
of this tuple must be equal to the width of the lines. Previously, we opened a
single line, which is why we use a single element tuple. If instead, we opened
2 lines, we would need to provide a 2 element tuple like below:

```erlang
> {ok, MultiLines} = gpio:open_lines(Chip, [1, 2, 5], [output], [0, 0, 0], "my-application").
{ok, #Ref<0.3061467712.2848194561.68339>}
> ok = gpio:write_lines(MultiLines, {1, 1, 1}). % Set all pins to 1
ok
> ok = gpio:write_lines(MultiLines, {1, 0, 1}). % Set IO1 and IO5 to 1, IO2 is set to 0
ok
```

## Monitoring line events

The following will monitor the pin `IO10` (`GPIO3_I10`) for rising values
(changes from 0 to 1).

```erlang
> {ok, LineEvents} = gpio:open_line_events(Chip, 10, [input], [rising_edge], "my-application").
{ok, {#Ref<0.3061467712.2848194561.68344>, <0.340.0>}}
```

The monitoring process will then receive messages like the following:

```erlang
{gpio, LineEvents, {event, Timestamp, Type}}
```

Where `Timestamp` is a system timestamp in nanosecond, and `Type` is either
`rising_edge` or `falling_edge`.

In case of an error, the process might receive a message like the following:

```erlang
{gpio, LineEvents, {error, Error}}
```