#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdatomic.h>
#include <assert.h>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>

#include <erl_nif.h>
#include <linux/gpio.h>

#define ARRAY_LENGTH(x) \
    ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))

/* Types */

typedef struct {
    ErlNifPid               owner;
    ErlNifMonitor           owner_monitor;

    atomic_flag             closed;

    int                     fd;
    bool                    open;
    char                    name[32];
    char                    label[32];
    unsigned int            lines;
} gpio_nif_chip_resource_t;

typedef struct {
    ErlNifPid               owner;
    ErlNifMonitor           owner_monitor;

    atomic_flag             closed;

    int                     fd;
    bool                    open;
    unsigned int            width; /* number of lines */

} gpio_nif_lines_resource_t;

typedef struct {
    ErlNifPid               owner;
    ErlNifMonitor           owner_monitor;

    atomic_flag             closed;

    int                     fd;
    bool                    open;
} gpio_nif_line_events_resource_t;


/* Atoms */

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_wait;

static ERL_NIF_TERM am_undefined;

static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;

static ERL_NIF_TERM am_name;
static ERL_NIF_TERM am_label;
static ERL_NIF_TERM am_lines;
static ERL_NIF_TERM am_closed;
static ERL_NIF_TERM am_rising_edge;
static ERL_NIF_TERM am_falling_edge;
static ERL_NIF_TERM am_input;
static ERL_NIF_TERM am_output;
static ERL_NIF_TERM am_active_low;
static ERL_NIF_TERM am_open_drain;
static ERL_NIF_TERM am_open_source;


/* Helpers */

static ERL_NIF_TERM errno_to_erl(ErlNifEnv* env, int error) {
    switch (error) {
        case EACCES: return enif_make_atom(env, "eacces");
        case EBADF: return enif_make_atom(env, "ebadf");
        case EINVAL: return enif_make_atom(env, "einval");
        case EISDIR: return enif_make_atom(env, "eisdir");
        case EMFILE: return enif_make_atom(env, "emfile");
        case ENFILE: return enif_make_atom(env, "enfile");
        case ENOENT: return enif_make_atom(env, "enoent");
        case ENOTDIR: return enif_make_atom(env, "enotdir");
        case ENOTTY: return enif_make_atom(env, "enotty");
        default: return enif_make_int(env, error);
    }
}

/* Resources */

/* Resource: chip */
static ErlNifResourceType* gpio_nif_chip_resource_type;

static gpio_nif_chip_resource_t* gpio_nif_chip_resource_new(ErlNifPid owner, int fd, const char* name, const char* label, unsigned int lines)
{
    gpio_nif_chip_resource_t *resource = (gpio_nif_chip_resource_t*) enif_alloc_resource(
        gpio_nif_chip_resource_type,
        sizeof(gpio_nif_chip_resource_t));
    resource->owner = owner;
    atomic_flag_clear(&(resource->closed));

    resource->fd = fd;
    resource->open = true;
    strncpy(resource->name, name, sizeof(resource->name));
    resource->name[sizeof(resource->name) - 1] = 0;
    strncpy(resource->label, label, sizeof(resource->label));
    resource->label[sizeof(resource->label) - 1] = 0;
    resource->lines = lines;

    return resource;
}

static void gpio_nif_chip_resource_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
    gpio_nif_chip_resource_t* resource = (gpio_nif_chip_resource_t*) obj;
    close(resource->fd);
    resource->fd = -1;
    resource->open = false;
}

static void gpio_nif_chip_resource_owner_down(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    gpio_nif_chip_resource_t* resource = (gpio_nif_chip_resource_t*) obj;

    if (!atomic_flag_test_and_set(&resource->closed)) {
        resource->open = false;
        assert (enif_select(env, resource->fd, ERL_NIF_SELECT_STOP, resource, NULL, am_undefined) >= 0);
    }
}

static ErlNifResourceTypeInit gpio_nif_chip_resource_callbacks = {
    .dtor = NULL,
    .stop = gpio_nif_chip_resource_stop,
    .down = gpio_nif_chip_resource_owner_down,
};

// /* Resource: lines */
static ErlNifResourceType* gpio_nif_lines_resource_type;

static gpio_nif_lines_resource_t* gpio_nif_lines_resource_new(ErlNifPid owner, int fd, unsigned int width)
{
    gpio_nif_lines_resource_t *resource = (gpio_nif_lines_resource_t*) enif_alloc_resource(
        gpio_nif_lines_resource_type,
        sizeof(gpio_nif_lines_resource_t));
    resource->owner = owner;
    atomic_flag_clear(&(resource->closed));

    resource->fd = fd;
    resource->open = true;
    resource->width = width;

    return resource;
}

static void gpio_nif_lines_resource_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
    gpio_nif_lines_resource_t* resource = (gpio_nif_lines_resource_t*) obj;
    close(resource->fd);
    resource->fd = -1;
    resource->open = false;
}

static void gpio_nif_lines_resource_owner_down(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    gpio_nif_lines_resource_t* resource = (gpio_nif_lines_resource_t*) obj;

    if (!atomic_flag_test_and_set(&resource->closed)) {
        resource->open = false;
        assert (enif_select(env, resource->fd, ERL_NIF_SELECT_STOP, resource, NULL, am_undefined) >= 0);
    }
}

static ErlNifResourceTypeInit gpio_nif_lines_resource_callbacks = {
    .dtor = NULL,
    .stop = gpio_nif_lines_resource_stop,
    .down = gpio_nif_lines_resource_owner_down,
};

/* Resource: line_events */
static ErlNifResourceType* gpio_nif_line_events_resource_type;

static gpio_nif_line_events_resource_t* gpio_nif_line_events_resource_new(ErlNifPid owner, int fd)
{
    gpio_nif_line_events_resource_t *resource = (gpio_nif_line_events_resource_t*) enif_alloc_resource(
        gpio_nif_line_events_resource_type,
        sizeof(gpio_nif_line_events_resource_t));
    resource->owner = owner;
    atomic_flag_clear(&(resource->closed));

    resource->fd = fd;
    resource->open = true;

    return resource;
}

static void gpio_nif_line_events_resource_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
    gpio_nif_line_events_resource_t* resource = (gpio_nif_line_events_resource_t*) obj;
    close(resource->fd);
    resource->fd = -1;
    resource->open = false;
}

static void gpio_nif_line_events_resource_owner_down(ErlNifEnv* env, void* obj, ErlNifPid* pid, ErlNifMonitor* monitor)
{
    gpio_nif_line_events_resource_t* resource = (gpio_nif_line_events_resource_t*) obj;

    if (!atomic_flag_test_and_set(&resource->closed)) {
        resource->open = false;
        assert (enif_select(env, resource->fd, ERL_NIF_SELECT_STOP, resource, NULL, am_undefined) >= 0);
    }
}

static ErlNifResourceTypeInit gpio_nif_line_events_resource_callbacks = {
    .dtor = NULL,
    .stop = gpio_nif_line_events_resource_stop,
    .down = gpio_nif_line_events_resource_owner_down,
};

/* API */

// chip_chip(Path)
static ERL_NIF_TERM gpio_nif_open_chip(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char path[4096];

    if (enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) <= 0) {
        return enif_make_badarg(env);
    }

    int fd;
    do {
        fd = open(path, O_RDWR);
    } while(fd == -1 && errno == EINTR);

    if (fd < 0) {
        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    struct gpiochip_info info = {0};
    if (ioctl(fd, GPIO_GET_CHIPINFO_IOCTL, &info) < 0) {
        close(fd);
        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    gpio_nif_chip_resource_t *chip_resource = gpio_nif_chip_resource_new(owner, fd, info.name, info.label, info.lines);
    if (enif_monitor_process(env, chip_resource, &owner, &chip_resource->owner_monitor)) {
        enif_release_resource(chip_resource);
        close(fd);

        return enif_make_badarg(env);
    }

    ERL_NIF_TERM result = enif_make_resource(env, chip_resource);
    enif_release_resource(chip_resource);

    return enif_make_tuple2(env, am_ok, result);
}

// chip_info(Chip)
static ERL_NIF_TERM gpio_nif_chip_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_chip_resource_t* chip_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_chip_resource_type, (void**) &chip_resource)) {
        return enif_make_badarg(env);
    }

    if (chip_resource->open) {
        // TODO: fetch lines and their info?
        ERL_NIF_TERM keys[] = {
            am_name,
            am_label,
            am_lines,
        };
        ERL_NIF_TERM values[] = {
            enif_make_string(env, chip_resource->name, ERL_NIF_LATIN1),
            enif_make_string(env, chip_resource->label, ERL_NIF_LATIN1),
            enif_make_uint(env, chip_resource->lines),
        };

        ERL_NIF_TERM result;
        static_assert(ARRAY_LENGTH(keys) == ARRAY_LENGTH(values), "key/value size mismatch");
        enif_make_map_from_arrays(env, keys, values, ARRAY_LENGTH(keys), &result);

        return enif_make_tuple2(env, am_ok, result);
    } else {
        return enif_make_tuple2(env, am_error, am_closed);
    }
}

// close_chip(Chip)
static ERL_NIF_TERM gpio_nif_close_chip(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_chip_resource_t* chip_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_chip_resource_type, (void**) &chip_resource)) {
        return enif_make_badarg(env);
    }

    if (!atomic_flag_test_and_set(&chip_resource->closed)) {
        chip_resource->open = false;
        assert (enif_select(env, chip_resource->fd, ERL_NIF_SELECT_STOP, chip_resource, NULL, am_undefined) >= 0);
        enif_demonitor_process(env, chip_resource, &chip_resource->owner_monitor);

        return am_ok;
    } else {
        return enif_make_tuple2(env, am_error, am_closed);
    }
}

// open_lines(Chip, Offsets, Flags, Defaults, ConsumerLabel)
static ERL_NIF_TERM gpio_nif_open_lines(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_chip_resource_t* chip_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_chip_resource_type, (void**) &chip_resource)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM offsets = argv[1];
    unsigned int offsets_length = 0;
    if (!enif_get_list_length(env, offsets, &offsets_length) || offsets_length < 1 || offsets_length > GPIOHANDLES_MAX) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM flags = argv[2];
    if (!enif_is_list(env, flags)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM defaults = argv[3];
    unsigned int defaults_length = 0;
    if (!enif_get_list_length(env, defaults, &defaults_length) || defaults_length != offsets_length) {
        return enif_make_badarg(env);
    }

    char consumer_label[32] = {0};
    if (!enif_get_string(env, argv[4], consumer_label, sizeof(consumer_label), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    struct gpiohandle_request req = {0};
    strncpy(req.consumer_label, consumer_label, sizeof(req.consumer_label));
    req.lines = offsets_length;
    {
        ERL_NIF_TERM offsets_list = offsets;
        ERL_NIF_TERM offset;
        ERL_NIF_TERM rest;
        unsigned int i = 0;
        while (enif_get_list_cell(env, offsets_list, &offset, &rest)) {
            unsigned int offset;
            if (!enif_get_uint(env, offset, &offset)) {
                return enif_make_badarg(env);
            }
            req.lineoffsets[i] = offset;

            i++;
            offsets_list = rest;
        }
    }

    {
        ERL_NIF_TERM flags_list = flags;
        ERL_NIF_TERM flag;
        ERL_NIF_TERM rest;
        while (enif_get_list_cell(env, flags_list, &flag, &rest)) {
            if (enif_is_identical(flag, am_input)) {
                req.flags |= GPIOHANDLE_REQUEST_INPUT;
            } else if (enif_is_identical(flag, am_output)) {
                req.flags |= GPIOHANDLE_REQUEST_OUTPUT;
            } else if (enif_is_identical(flag, am_active_low)) {
                req.flags |= GPIOHANDLE_REQUEST_ACTIVE_LOW;
            } else if (enif_is_identical(flag, am_open_drain)) {
                req.flags |= GPIOHANDLE_REQUEST_OPEN_DRAIN;
            } else if (enif_is_identical(flag, am_open_source)) {
                req.flags |= GPIOHANDLE_REQUEST_OPEN_SOURCE;
            }

            flags_list = rest;
        }
    }

    {
        ERL_NIF_TERM defaults_list = defaults;
        ERL_NIF_TERM default_;
        ERL_NIF_TERM rest;
        unsigned int i = 0;
        while (enif_get_list_cell(env, defaults_list, &default_, &rest)) {
            unsigned int active;
            if (!enif_get_uint(env, default_, &active)) {
                return enif_make_badarg(env);
            }
            req.default_values[i] = active != 0;

            i++;
            defaults_list = rest;
        }
    }

    if (ioctl(chip_resource->fd, GPIO_GET_LINEHANDLE_IOCTL, &req) < 0) {
        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    gpio_nif_lines_resource_t *lines_resource = gpio_nif_lines_resource_new(owner, req.fd, offsets_length);
    if (enif_monitor_process(env, lines_resource, &owner, &lines_resource->owner_monitor)) {
        enif_release_resource(lines_resource);
        close(req.fd);

        return enif_make_badarg(env);
    }

    ERL_NIF_TERM result = enif_make_resource(env, lines_resource);
    enif_release_resource(lines_resource);

    return enif_make_tuple2(env, am_ok, result);
}

// read_lines(Lines)
static ERL_NIF_TERM gpio_nif_read_lines(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_lines_resource_t* lines_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_lines_resource_type, (void**) &lines_resource)) {
        return enif_make_badarg(env);
    }

    struct gpiohandle_data req = {0};
    if (ioctl(lines_resource->fd, GPIOHANDLE_GET_LINE_VALUES_IOCTL, &req) < 0) {
        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    ERL_NIF_TERM values[lines_resource->width];
    for (unsigned int i = 0; i < lines_resource->width; i++) {
        values[i] = enif_make_uint(env, req.values[i]);
    }

    return enif_make_tuple2(env, am_ok, enif_make_tuple_from_array(env, values, lines_resource->width));
}

// write_lines(Lines, Values :: [uint])
static ERL_NIF_TERM gpio_nif_write_lines(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_lines_resource_t* lines_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_lines_resource_type, (void**) &lines_resource)) {
        return enif_make_badarg(env);
    }

    const ERL_NIF_TERM* values;
    int values_length;
    if (!enif_get_tuple(env, argv[1], &values_length, &values) || values_length != lines_resource->width) {
        return enif_make_badarg(env);
    }

    struct gpiohandle_data req = {0};
    for (unsigned int i = 0; i < lines_resource->width; i++) {
        unsigned int value;
        if (!enif_get_uint(env, values[i], &value)) {
            return enif_make_badarg(env);
        }
        req.values[i] = (uint8_t) value;
    }

    if (ioctl(lines_resource->fd, GPIOHANDLE_SET_LINE_VALUES_IOCTL, &req) < 0) {
        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    return am_ok;
}

// close_lines(Lines)
static ERL_NIF_TERM gpio_nif_close_lines(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_lines_resource_t* lines_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_lines_resource_type, (void**) &lines_resource)) {
        return enif_make_badarg(env);
    }

    if (!atomic_flag_test_and_set(&lines_resource->closed)) {
        lines_resource->open = false;
        assert (enif_select(env, lines_resource->fd, ERL_NIF_SELECT_STOP, lines_resource, NULL, am_undefined) >= 0);
        enif_demonitor_process(env, lines_resource, &lines_resource->owner_monitor);

        return am_ok;
    } else {
        return enif_make_tuple2(env, am_error, am_closed);
    }
}

// open_line_events(Chip, Offset, HandleFlags, EventFlags, ConsumerLabel)
static ERL_NIF_TERM gpio_nif_open_line_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_chip_resource_t* chip_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_chip_resource_type, (void**) &chip_resource)) {
        return enif_make_badarg(env);
    }

    unsigned int offset;
    if (!enif_get_uint(env, argv[1], &offset)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM handle_flags = argv[2];
    if (!enif_is_list(env, handle_flags)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM event_flags = argv[3];
    if (!enif_is_list(env, event_flags)) {
        return enif_make_badarg(env);
    }

    char consumer_label[32] = {0};
    if (!enif_get_string(env, argv[4], consumer_label, sizeof(consumer_label), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    struct gpioevent_request req = {0};
    req.lineoffset = (uint32_t) offset;
    strncpy(req.consumer_label, consumer_label, sizeof(req.consumer_label));
    req.handleflags = GPIOHANDLE_REQUEST_INPUT;
    {
        ERL_NIF_TERM handle_flags_list = handle_flags;
        ERL_NIF_TERM handle_flag;
        ERL_NIF_TERM rest;

        while (enif_get_list_cell(env, handle_flags_list, &handle_flag, &rest)) {
            if (enif_compare(handle_flag, am_active_low) == 0) {
                req.handleflags |= GPIOHANDLE_REQUEST_ACTIVE_LOW;
            } else if (enif_compare(handle_flag, am_open_drain) == 0) {
                req.handleflags |= GPIOHANDLE_REQUEST_OPEN_DRAIN;
            } else if (enif_compare(handle_flag, am_open_source) == 0) {
                req.handleflags |= GPIOHANDLE_REQUEST_OPEN_SOURCE;
            }

            handle_flags_list = rest;
        }
    }

    req.eventflags = 0;
    {
        ERL_NIF_TERM event_flags_list = event_flags;
        ERL_NIF_TERM event_flag;
        ERL_NIF_TERM rest;

        while (enif_get_list_cell(env, event_flags_list, &event_flag, &rest)) {
            if (enif_compare(event_flag, am_rising_edge) == 0) {
                req.eventflags |= GPIOEVENT_REQUEST_RISING_EDGE;
            } else if (enif_compare(event_flag, am_falling_edge) == 0) {
                req.eventflags |= GPIOEVENT_REQUEST_FALLING_EDGE;
            }

            event_flags_list = rest;
        }
    }

    if (ioctl(chip_resource->fd, GPIO_GET_LINEEVENT_IOCTL, &req) < 0) {
        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    int flags = fcntl(req.fd, F_GETFL, 0);
    if (flags == -1 || fcntl(req.fd, F_SETFL, flags | O_NONBLOCK)) {
        close(req.fd);
        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    ErlNifPid owner;
    enif_self(env, &owner);

    gpio_nif_line_events_resource_t *line_events_resource = gpio_nif_line_events_resource_new(owner, req.fd);
    if (enif_monitor_process(env, line_events_resource, &owner, &line_events_resource->owner_monitor)) {
        enif_release_resource(line_events_resource);
        close(req.fd);

        return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
    }

    ERL_NIF_TERM result = enif_make_resource(env, line_events_resource);
    enif_release_resource(line_events_resource);

    return enif_make_tuple2(env, am_ok, result);
}

// read_line_events(LineEvents, Ref)
static ERL_NIF_TERM gpio_nif_read_line_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_line_events_resource_t* line_events_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_line_events_resource_type, (void**) &line_events_resource)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM ref = argv[1];
    if (!enif_is_ref(env, ref)) {
        return enif_make_badarg(env);
    }

    struct gpioevent_data event;
    int read_result = read(line_events_resource->fd, &event, sizeof(event));
    if (read_result == sizeof(event)) {
        unsigned long timestamp = enif_make_uint64(env, event.timestamp);
        ERL_NIF_TERM type = event.id == GPIOEVENT_EVENT_RISING_EDGE
            ? am_rising_edge
            : am_falling_edge;
        ERL_NIF_TERM result = enif_make_tuple2(env, timestamp, type);

        return enif_make_tuple2(env, am_ok, result);
    } else if (read_result < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            assert (enif_select(env, line_events_resource->fd, ERL_NIF_SELECT_READ, line_events_resource, NULL, ref) >= 0);
            return enif_make_tuple2(env, am_wait, ref);
        } else {
            return enif_make_tuple2(env, am_error, errno_to_erl(env, errno));
        }
    } else if (read_result == 0) {
        return enif_make_tuple2(env, am_error, am_closed);
    } else {
        return enif_make_tuple2(env, am_error, errno_to_erl(env, EIO));
    }
}

// close_line_events(LineEvents)
static ERL_NIF_TERM gpio_nif_close_line_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    gpio_nif_line_events_resource_t* line_events_resource;
    if (!enif_get_resource(env, argv[0], gpio_nif_line_events_resource_type, (void**) &line_events_resource)) {
        return enif_make_badarg(env);
    }

    if (!atomic_flag_test_and_set(&line_events_resource->closed)) {
        line_events_resource->open = false;
        assert (enif_select(env, line_events_resource->fd, ERL_NIF_SELECT_STOP, line_events_resource, NULL, am_undefined) >= 0);
        enif_demonitor_process(env, line_events_resource, &line_events_resource->owner_monitor);

        return am_ok;
    } else {
        return enif_make_tuple2(env, am_error, am_closed);
    }
}

/* NIF Callbacks */

static int on_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");
    am_wait = enif_make_atom(env, "wait");

    am_undefined = enif_make_atom(env, "undefined");

    am_true = enif_make_atom(env, "true");
    am_false = enif_make_atom(env, "false");

    am_name = enif_make_atom(env, "name");
    am_label = enif_make_atom(env, "label");
    am_lines = enif_make_atom(env, "lines");
    am_closed = enif_make_atom(env, "closed");
    am_rising_edge = enif_make_atom(env, "rising_edge");
    am_falling_edge = enif_make_atom(env, "falling_edge");
    am_input = enif_make_atom(env, "input");
    am_output = enif_make_atom(env, "output");
    am_active_low = enif_make_atom(env, "active_low");
    am_open_drain = enif_make_atom(env, "open_drain");
    am_open_source = enif_make_atom(env, "open_source");

    // Resources
    gpio_nif_chip_resource_type = enif_open_resource_type_x(env,
        "chip",
        &gpio_nif_chip_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    gpio_nif_lines_resource_type = enif_open_resource_type_x(env,
        "lines",
        &gpio_nif_lines_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    gpio_nif_line_events_resource_type = enif_open_resource_type_x(env,
        "line_events",
        &gpio_nif_line_events_resource_callbacks,
        ERL_NIF_RT_CREATE,
        NULL);

    *priv_data = NULL;

    return 0;
}


static void on_unload(ErlNifEnv *env, void* priv_data)
{
}


static int on_upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (on_load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}


static ErlNifFunc nif_funcs[] = {
    {"open_chip_nif", 1, gpio_nif_open_chip, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"chip_info_nif", 1, gpio_nif_chip_info},
    {"close_chip_nif", 1, gpio_nif_close_chip},
    {"open_lines_nif", 5, gpio_nif_open_lines, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_lines_nif", 1, gpio_nif_read_lines, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write_lines_nif", 2, gpio_nif_write_lines, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_lines_nif", 1, gpio_nif_close_lines},
    {"open_line_events_nif", 5, gpio_nif_open_line_events, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_line_events_nif", 2, gpio_nif_read_line_events},
    {"close_line_events_nif", 1, gpio_nif_close_line_events},
};

ERL_NIF_INIT(gpio, nif_funcs, on_load, NULL, on_upgrade, on_unload);