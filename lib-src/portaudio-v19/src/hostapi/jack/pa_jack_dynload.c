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

#define PADL_DEFINE_POINTERS
#include "pa_dynload.h"
#include "pa_jack_dynload.h"

#if defined(PA_DYNAMIC_JACK)
static paDynamicLib jacklib = NULL;
#endif

int PaJack_Load(void)
{
#if !defined(PA_DYNAMIC_JACK)
    return 1;
#else

#if defined(__APPLE__)
    jacklib = PaDL_Load("libjack.dylib");
#elif defined(_WIN64)
    jacklib = PaDL_Load("libjack64.dll");
#elif defined(WIN32)
    jacklib = PaDL_Load("libjack.dll");
#else
    jacklib = PaDL_Load("libjack.so");
#endif

    if (!jacklib) {
        return 0;
    }

    PADL_FINDSYMBOL(jacklib, jack_client_name_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_client_name_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_ports, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_sample_rate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_ports, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_by_name, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_latency, goto error);
    PADL_FINDSYMBOL(jacklib, jack_free, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_ports, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_by_name, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_latency, goto error);
    PADL_FINDSYMBOL(jacklib, jack_free, goto error);
    PADL_FINDSYMBOL(jacklib, jack_client_open, goto error);
    PADL_FINDSYMBOL(jacklib, jack_on_shutdown, goto error);
    PADL_FINDSYMBOL(jacklib, jack_set_error_function, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_buffer_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_set_sample_rate_callback, goto error);
    PADL_FINDSYMBOL(jacklib, jack_set_xrun_callback, goto error);
    PADL_FINDSYMBOL(jacklib, jack_set_process_callback, goto error);
    PADL_FINDSYMBOL(jacklib, jack_activate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_deactivate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_client_close, goto error);
    PADL_FINDSYMBOL(jacklib, jack_deactivate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_client_close, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_sample_rate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_unregister, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_unregister, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_name_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_client_name_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_sample_rate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_sample_rate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_name_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_register, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_name_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_register, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_ports, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_by_name, goto error);
    PADL_FINDSYMBOL(jacklib, jack_free, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_ports, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_by_name, goto error);
    PADL_FINDSYMBOL(jacklib, jack_free, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_latency, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_buffer_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_latency, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_buffer_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_frame_time, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_sample_rate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_frame_time, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_latency, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_latency, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_buffer, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_buffer, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_sample_rate, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_get_buffer, goto error);
    PADL_FINDSYMBOL(jacklib, jack_connect, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_name, goto error);
    PADL_FINDSYMBOL(jacklib, jack_connect, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_name, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_connected, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_disconnect, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_connected, goto error);
    PADL_FINDSYMBOL(jacklib, jack_port_disconnect, goto error);
    PADL_FINDSYMBOL(jacklib, jack_frame_time, goto error);
    PADL_FINDSYMBOL(jacklib, jack_client_name_size, goto error);
    PADL_FINDSYMBOL(jacklib, jack_get_client_name, goto error);

    return 1;

error:

    PaJack_Unload();

    return 0;
#endif
}

void PaJack_Unload(void)
{
#if defined(PA_DYNAMIC_JACK)
    if (jacklib) {
        PaDL_Unload(jacklib);
        jacklib = NULL;
    }
#endif
}
