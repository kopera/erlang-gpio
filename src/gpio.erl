-module(gpio).
-export([
    open_chip/1,
    chip_info/1,
    close_chip/1
]).
-export([
    open_lines/5,
    read_lines/1,
    write_lines/2,
    close_lines/1
]).
-export([
    open_line_events/5,
    close_line_events/1
]).
-export_type([
    chip/0,
    lines/0,
    line_events/0
]).

-on_load(init/0).


%% @doc Open a GPIO chip given its full path. The path should normally look like
%% "/dev/gpiochipX".
%%
%% Once a chip is open, it can be used to query its info using {@link chip_info/1},
%% open lines for reading/writing using {@link open_lines/5} or
%% monitor lines for input events using {@link open_line_events/5}.
%%
%% The {@link chip(). handle} to the GPIO chip is automatically closed when the
%% process that opened it terminates, it can also be closed explicitely
%% using {@link close_chip/1}.
%%
%% Returns a {@link chip(). handle} to use the GPIO chip in case of success.
-spec open_chip(Path) -> {ok, chip()} | {error, term()} when
    Path :: file:filename_all().
-opaque chip() :: reference().
open_chip(Path) ->
    open_chip_nif(unicode:characters_to_list(Path)).


%% nif
open_chip_nif(_Path) ->
    erlang:nif_error(not_loaded).


%% @doc Return information about the open GPIO chip. This includes the name,
%% label and number of available lines.
-spec chip_info(chip()) -> {ok, chip_info()} | {error, term()}.
-type chip_info() :: #{
    name := string(),
    label := string(),
    lines := non_neg_integer()
}.
chip_info(Chip) ->
    chip_info_nif(Chip).


%% nif
chip_info_nif(_Chip) ->
    erlang:nif_error(not_loaded).


%% @doc Close a previously open GPIO chip.
-spec close_chip(chip()) -> ok | {error, term()}.
close_chip(Chip) ->
    close_chip_nif(Chip).


%% nif
close_chip_nif(_Chip) ->
    erlang:nif_error(not_loaded).


%% @doc Open a set of lines for reading/writing.
%%
%% The lines to be opened are specified through the `Offsets' parameter, for
%% example to open the 16th, 20th and 21st lines Offsets would be `[16, 20, 21]'.
%% these offsets are board and system specific, make sure to consult the
%% documentation of the system to avoid damages.
%%
%% The `Flags' allow specifying the pin direction, the active state as well as
%% the drive mode.
%%
%% The `Defaults' parameter specifies the default pin values. The length of this
%% list must be equal to the length of the `Offsets` list.
%% 
%% The `ConsumerLabel' should be an ASCII string of the application name. This
%% string will be truncated to 31 characters.
%% 
%% Returns a {@link lines(). handle} to read from using {@link read_lines/1}; or
%% write to using {@link write_lines/2}; the opened lines in case of success.
-spec open_lines(chip(), [Offset], [Flag], [Default], ConsumerLabel) -> {ok, lines()} | {error, term()} when
    Offset :: non_neg_integer(),
    Flag :: input | output | active_low | open_drain | open_source,
    Default :: 0 | 1,
    ConsumerLabel :: string() | binary().
-opaque lines() :: reference().
open_lines(Chip, Offsets, Flags, Defaults, ConsumerLabel) when length(Offsets) =< 64, length(Offsets) =:= length(Defaults) ->
    open_lines_nif(Chip, Offsets, Flags, Defaults, unicode:characters_to_list(ConsumerLabel)).


%% nif
open_lines_nif(_Chip, _Offsets, _Flags, _Defaults, _ConsumerLabel) ->
    erlang:nif_error(not_loaded).


%% @doc Read the current value of a set of opened lines.
%%
%% Returns a tuple of `0' and `1' values. The tuple size is equal to the number
%% of lines opened using {@link open_lines/5}.
-spec read_lines(lines()) -> {ok, tuple()} | {error, term()}.
read_lines(Lines) ->
    read_lines_nif(Lines).


%% nif
read_lines_nif(_Lines) ->
    erlang:nif_error(not_loaded).


%% @doc Set the current value of a set of opened lines.
%%
%% The `Values' tuple must consiste of `0' and `1' values and its size must be
%% equal to the number of lines opened using {@link open_lines/5}.
-spec write_lines(lines(), tuple()) -> ok | {error, term()}.
write_lines(Lines, Values) ->
    write_lines_nif(Lines, Values).


%% nif
write_lines_nif(_Lines, _Values) ->
    erlang:nif_error(not_loaded).


%% @doc Close a previously opened lines.
-spec close_lines(lines()) -> ok | {error, term()}.
close_lines(Lines) ->
    close_lines_nif(Lines).


%% nif
close_lines_nif(_Lines) ->
    erlang:nif_error(not_loaded).


%% @doc Open a set of lines for montoring.
%%
%% The lines to be opened are specified through the `Offsets' parameter, for
%% example to open the 16th, 20th and 21st lines Offsets would be `[16, 20, 21]'.
%% these offsets are board and system specific.
%%
%% The `Flags' allow specifying the active state as well as the drive mode.
%% 
%% The `EventFlags' allow specifying how events are detected.
%%
%% The `ConsumerLabel' should be an ASCII string of the application name. This
%% string will be truncated to 31 characters.
%% 
%% Upon opening the lines for monitoring, the owning process will start
%% receiving messages in the form of:
%%
%% ```
%%      {gpio, LineEventsHandle, {event, Timestamp, Type}}
%% '''
%% 
%% Where `LineEventsHandle' is the handle returned by this function, `Timestamp'
%% is best estimate of time of event occurrence, in nanoseconds and `Type' is
%% one of `rising_edge' or `falling_edge'.
%% 
%% Returns a {@link lines(). handle} that can be used for ending the lines
%% monitoring using {@link close_line_events/1}.
-spec open_line_events(chip(), Offset, [Flag], [EventFlag], ConsumerLabel) -> {ok, line_events()} | {error, term()} when
    Offset :: non_neg_integer(),
    Flag :: active_low | open_drain | open_source,
    EventFlag :: rising_edge | falling_edge,
    ConsumerLabel :: string() | binary().
-opaque line_events() :: {reference(), pid()}.
open_line_events(Chip, Offset, HandleFlags, EventFlags, ConsumerLabel) ->
    case open_line_events_nif(Chip, Offset, HandleFlags, EventFlags, unicode:characters_to_list(ConsumerLabel)) of
        {ok, LineEvents} ->
            Owner = self(),
            Receiver = spawn_link(fun() ->
                Ref = make_ref(),
                line_events_loop(LineEvents, Offset, Ref, Owner)
            end),
            {ok, {LineEvents, Receiver}};
        {error, _} = Error ->
            Error
    end.


%% nif
open_line_events_nif(_Chip, _Offsets, _HandleFlags, _EventFlags, _ConsumerLabel) ->
    erlang:nif_error(not_loaded).


%% @private
line_events_loop(LineEvents, Offset, Ref, Owner) ->
    case read_line_events_nif(LineEvents, Ref) of
        {wait, Ref} ->
            receive
                {select, LineEvents, Ref, ready_input} ->
                    line_events_loop(LineEvents, Offset, Ref, Owner);
                {close, Closer, LineEvents} ->
                    ok = close_line_events_nif(LineEvents),
                    Closer ! {closed, LineEvents}
            end;
        {ok, {Timestamp, Type}} ->
            Owner ! {gpio, {LineEvents, self()}, {event, Timestamp, Type}},
            line_events_loop(LineEvents, Offset, Ref, Owner);
        {error, Reason} ->
            Owner ! {gpio, {LineEvents, self()}, {error, Reason}}
    end.


%% nif
read_line_events_nif(_LineEvents, _Ref) ->
    erlang:nif_error(not_loaded).


%% @doc Close a previously opened line events.
-spec close_line_events(line_events()) -> ok | {error, term()}.
close_line_events({LineEvents, Receiver}) ->
    case is_process_alive(Receiver) of
        true ->
            Receiver ! {close, self(), LineEvents},
            receive
                {closed, LineEvents} ->
                    ok
            after
                5000 ->
                    exit(timeout)
            end;
        false ->
            {error, closed}
    end.


%% nif
close_line_events_nif(_LineEvents) ->
    erlang:nif_error(not_loaded).


init() ->
    case nif_path() of
        undefined ->
            ok;
        Path ->
            ok = erlang:load_nif(Path, 0)
    end.


-spec nif_path() -> file:filename_all() | undefined.
nif_path() ->
    Priv = case code:priv_dir(gpio) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                File when is_list(File) ->
                    filename:join([filename:dirname(File), "../priv"]);
                _ ->
                    "../priv"
            end;
        Dir ->
            Dir
    end,
    nif_path(os:type(), Priv).


nif_path({unix, linux}, Dir) ->
    filename:join([Dir, "gpio_nif"]);

nif_path(_, _Dir) ->
    undefined.
