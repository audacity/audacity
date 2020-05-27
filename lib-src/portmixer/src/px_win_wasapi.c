/*
 * PortMixer
 * Windows DirectSound Implementation
 *
 * Copyright (c) 2002, 2006
 *
 * Written by Dominic Mazzoni and Augustus Saunders
 *        and Leland Lucius
 *
 * PortMixer is intended to work side-by-side with PortAudio,
 * the Portable Real-Time Audio Library by Ross Bencina and
 * Phil Burk.
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
 *
 */
#define COBJMACROS

#include <windows.h>

#include "portaudio.h"
#include "pa_win_wasapi.h"

#include "portmixer.h"
#include "px_mixer.h"
#include "px_win_common.h"

#if defined(_DEBUG)
#include <stdio.h>
static void dprintf(const char *format, ...)
{
   char buf[4096];
   va_list args;
   int cnt;

   va_start(args, format);
   cnt = _vsnprintf(buf, sizeof(buf) - 1, format, args);
   va_end(args);

   if (cnt > 0) {
      buf[cnt] = '\0';
      OutputDebugString(buf);
   }
}
#endif

int OpenMixer_Win_WASAPI(px_mixer *Px, int index)
{
   const wchar_t *deviceIn = PaWasapi_GetInputDeviceID(Px->pa_stream);
   const wchar_t *deviceOut = PaWasapi_GetOutputDeviceID(Px->pa_stream);
   int ret = FALSE;

   if (open_ep_mixers_byid(Px, deviceIn, deviceOut))
   {
      ret = TRUE;
   }

   return ret;
}
