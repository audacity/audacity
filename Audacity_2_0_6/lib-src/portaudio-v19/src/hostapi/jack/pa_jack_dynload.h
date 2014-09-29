/*
 * $Id: pa_jack.c 1668 2011-05-02 17:07:11Z rossb $
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 * JACK Implementation by Joshua Haberman
 *
 * Copyright (c) 2004 Stefan Westerfeld <stefan@space.twc.de>
 * Copyright (c) 2004 Arve Knudsen <aknuds-1@broadpark.no>
 * Copyright (c) 2002 Joshua Haberman <joshua@haberman.com>
 *
 * Based on the Open Source API proposed by Ross Bencina
 * Copyright (c) 1999-2002 Ross Bencina, Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however,
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also
 * requested that these non-binding requests be included along with the
 * license above.
 */

/**
 @file
 @ingroup hostapi_src
*/
#ifndef INCLUDED_PA_JACK_DYNLINK_H
#define INCLUDED_PA_JACK_DYNLINK_H

#include <jack/jack.h>

#include "pa_dynload.h"

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

#if defined(PA_DYNAMIC_JACK)

PADL_FUNC_WITH_RETURN(
    int,
    jack_activate,
    (jack_client_t *client),
    (client)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_client_close,
    (jack_client_t *client),
    (client)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_client_name_size,
    (void),
    ()
);
PADL_FUNC_WITH_RETURN(
    jack_client_t *,
    jack_client_open,
    (const char *client_name, jack_options_t options, jack_status_t *status, ...),
    (client_name, options, status)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_connect,
    (jack_client_t *client, const char *source_port, const char *destination_port),
    (client, source_port, destination_port)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_deactivate,
    (jack_client_t *client),
    (client)
);
PADL_FUNC_WITH_RETURN(
    jack_nframes_t,
    jack_frame_time,
    (const jack_client_t *client),
    (client)
);
PADL_FUNC_NO_RETURN(
    jack_free,
    (void *ptr),
    (ptr)
);
PADL_FUNC_WITH_RETURN(
    jack_nframes_t,
    jack_get_buffer_size,
    (jack_client_t *client),
    (client)
);
PADL_FUNC_WITH_RETURN(
    char *,
    jack_get_client_name,
    (jack_client_t *client),
    (client)
);
PADL_FUNC_WITH_RETURN(
    const char **,
    jack_get_ports,
    (jack_client_t *client, const char *port_name_pattern, const char *type_name_pattern, unsigned long flags),
    (client, port_name_pattern, type_name_pattern, flags)
);
PADL_FUNC_WITH_RETURN(
    jack_nframes_t,
    jack_get_sample_rate,
    (jack_client_t *client),
    (client)
);
PADL_FUNC_NO_RETURN(
    jack_on_shutdown,
    (jack_client_t *client, JackShutdownCallback shutdown_callback, void *arg),
    (client, shutdown_callback, arg)
);
PADL_FUNC_WITH_RETURN(
    jack_port_t *,
    jack_port_by_name,
    (jack_client_t *client, const char *port_name),
    (client, port_name)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_port_connected,
    (const jack_port_t *port),
    (port)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_port_disconnect,
    (jack_client_t *client, jack_port_t *port),
    (client, port)
);
PADL_FUNC_WITH_RETURN(
    void *,
    jack_port_get_buffer,
    (jack_port_t *port, jack_nframes_t frames),
    (port, frames)
);
PADL_FUNC_WITH_RETURN(
    jack_nframes_t,
    jack_port_get_latency,
    (jack_port_t *port),
    (port)
);
PADL_FUNC_WITH_RETURN(
    const char *,
    jack_port_name,
    (const jack_port_t *port),
    (port)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_port_name_size,
    (void),
    ()
);
PADL_FUNC_WITH_RETURN(
    jack_port_t *,
    jack_port_register,
    (jack_client_t *client, const char *port_name, const char *port_type, unsigned long flags, unsigned long buffer_size),
    (client, port_name, port_type, flags, buffer_size)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_port_unregister,
    (jack_client_t *client, jack_port_t *port),
    (client, port)
);
PADL_FUNC_NO_RETURN(
    jack_set_error_function,
    (void (*func)(const char *)),
    (func)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_set_process_callback,
    (jack_client_t *client, JackProcessCallback process_callback, void *arg),
    (client, process_callback, arg)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_set_sample_rate_callback,
    (jack_client_t *client, JackSampleRateCallback srate_callback, void *arg),
    (client, srate_callback, arg)
);
PADL_FUNC_WITH_RETURN(
    int,
    jack_set_xrun_callback,
    (jack_client_t *client, JackXRunCallback xrun_callback, void *arg),
    (client, xrun_callback, arg)
);
#endif /* PA_DYNAMIC_JACK */

int PaJack_Load(void);
void PaJack_Unload(void);

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* INCLUDED_PA_JACK_DYNLINK_H */
