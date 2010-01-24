#ifndef PA_TRACE_H
#define PA_TRACE_H
/*
 * $Id: pa_trace.h,v 1.5 2003-03-02 08:01:35 dmazzoni Exp $
 * Portable Audio I/O Library Trace Facility
 * Store trace information in real-time for later printing.
 *
 * Based on the Open Source API proposed by Ross Bencina
 * Copyright (c) 1999-2000 Phil Burk
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
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */


#define TRACE_REALTIME_EVENTS     (0)   /* Keep log of various real-time events. */
#define MAX_TRACE_RECORDS      (2048)

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */


    /************************************************************************************/
    /****************** Prototypes ******************************************************/
    /************************************************************************************/

#if TRACE_REALTIME_EVENTS

    void DumpTraceMessages();
    void ResetTraceMessages();
    void AddTraceMessage( char *msg, int data );

#else

#define AddTraceMessage(msg,data) /* noop */
#define ResetTraceMessages() /* noop */
#define DumpTraceMessages() /* noop */

#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* PA_TRACE_H */
