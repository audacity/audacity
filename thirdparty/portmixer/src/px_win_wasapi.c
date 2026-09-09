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
#include <mmdeviceapi.h>

#include "portaudio.h"
#include "pa_win_wasapi.h"

#include "portmixer.h"
#include "px_mixer.h"

static const wchar_t* GetDeviceID(PaDeviceIndex index)
{
   const wchar_t* result = NULL;

   if (index >= 0)
   {
      IMMDevice* immDevice;
      PaWasapi_GetIMMDevice(index, &immDevice);

      wchar_t* deviceID;

      if (!FAILED(IMMDevice_GetId(immDevice, &deviceID)))
      {
         result = _wcsdup(deviceID);
         CoTaskMemFree(deviceID);
      }
   }

   return result;
}

int OpenMixer_Win_WASAPI(px_mixer *Px, int index)
{
   const wchar_t* deviceIn = GetDeviceID(Px->input_device_index);
   const wchar_t *deviceOut = GetDeviceID(Px->output_device_index);

   return open_ep_mixers_byid(Px, deviceIn, deviceOut);
}
