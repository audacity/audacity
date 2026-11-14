/*
 * PortMixer
 * Common Windows routines for Vista/7
 *
 * Copyright (c) 2002, 2009
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
#define CINTERFACE 1
#define COBJMACROS 1

#include <windows.h>
#include <tchar.h>
#include <strsafe.h>
#include <mmdeviceapi.h>
#include <initguid.h>
#include <endpointvolume.h>

#include "portaudio.h"

#include "portmixer.h"
#include "px_mixer.h"
#include "px_win_endpoint.h"

#if defined(_DEBUG)
#include <stdio.h>
#include <stdlib.h>
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

DEFINE_GUID(CLSID_MMDeviceEnumerator, 0xBCDE0395, 0xE52F, 0x467C,
            0x8E, 0x3D, 0xC4, 0x57, 0x92, 0x91, 0x69, 0x2E);
DEFINE_GUID(IID_IMMDeviceEnumerator, 0xA95664D2, 0x9614, 0x4F35,
            0xA7, 0x46, 0xDE, 0x8D, 0xB6, 0x36, 0x17, 0xE6);
DEFINE_GUID(IID_IAudioEndpointVolume, 0x5CDF2C82, 0x841E, 0x4546,
            0x97, 0x22, 0x0C, 0xF7, 0x40, 0x78, 0x22, 0x9A);

#define DRV_RESERVED                      0x0800
#define DRV_QUERYFUNCTIONINSTANCEID       (DRV_RESERVED + 17)
#define DRV_QUERYFUNCTIONINSTANCEIDSIZE   (DRV_RESERVED + 18)

int open_ep_mixers(px_mixer *Px, UINT deviceIn, UINT deviceOut)
{
   PxEPInfo *info;
   IMMDeviceEnumerator *denum = NULL;
   IMMDevice *device = NULL;
   HRESULT hr;
   MMRESULT res;
   LPWSTR idStr;
   DWORD idLen;

   if (!initialize(Px)) {
      goto fail;
   }

   info = (PxEPInfo *) Px->info;
   info->inputEP = NULL;
   info->outputEP = NULL;

   // Create an audio endpoint device enumerator.
   hr = CoCreateInstance(&CLSID_MMDeviceEnumerator,
                         NULL,
                         CLSCTX_ALL,
                         &IID_IMMDeviceEnumerator,
                         &denum);
   if (FAILED(hr)) {
      goto fail;
   }

   if (deviceIn == WAVE_MAPPER) {
      hr = IMMDeviceEnumerator_GetDefaultAudioEndpoint(denum,
                                                       eCapture,
                                                       eMultimedia,
                                                       &device);
      if (SUCCEEDED(hr)) {
         hr = IMMDevice_Activate(device,
                                 &IID_IAudioEndpointVolume,
                                 CLSCTX_ALL,
                                 NULL,
                                 &info->inputEP);
         IUnknown_Release(device);
      }

      if (FAILED(hr)) {
         goto fail;
      }
   }
   else {
      res = waveInMessage((HWAVEIN)IntToPtr(deviceIn),
                          DRV_QUERYFUNCTIONINSTANCEIDSIZE,
                          (DWORD_PTR)&idLen,
                          (DWORD_PTR)NULL);
      if (res != MMSYSERR_NOERROR) {
         goto fail;
      }

      idStr = (WCHAR *) CoTaskMemAlloc(idLen + sizeof(WCHAR));
      if (idStr == NULL) {
         goto fail;
      }

      res = waveInMessage((HWAVEIN)IntToPtr(deviceIn),
                          DRV_QUERYFUNCTIONINSTANCEID,
                          (DWORD_PTR)idStr,
                          (DWORD_PTR)idLen);
      if (res != MMSYSERR_NOERROR) {
         CoTaskMemFree(idStr);
         goto fail;
      }

      hr = IMMDeviceEnumerator_GetDevice(denum, idStr, &device);
      if (SUCCEEDED(hr)) {
         hr = IMMDevice_Activate(device,
                                 &IID_IAudioEndpointVolume,
                                 CLSCTX_ALL,
                                 NULL,
                                 &info->inputEP);
         IUnknown_Release(device);
      }

      CoTaskMemFree(idStr);

      if (FAILED(hr)) {
         goto fail;
      }
   }

   if (deviceOut == WAVE_MAPPER) {
      hr = IMMDeviceEnumerator_GetDefaultAudioEndpoint(denum,
                                                       eRender,
                                                       eMultimedia,
                                                       &device);
      if (SUCCEEDED(hr)) {
         hr = IMMDevice_Activate(device,
                                 &IID_IAudioEndpointVolume,
                                 CLSCTX_ALL,
                                 NULL,
                                 &info->outputEP);
         IUnknown_Release(device);
      }

      if (FAILED(hr)) {
         goto fail;
      }
   }
   else {
      res = waveOutMessage((HWAVEOUT)IntToPtr(deviceOut),
                           DRV_QUERYFUNCTIONINSTANCEIDSIZE,
                           (DWORD_PTR)&idLen,
                           (DWORD_PTR)NULL);
      if (res != MMSYSERR_NOERROR) {
         goto fail;
      }

      idStr = (WCHAR *) CoTaskMemAlloc(idLen + sizeof(WCHAR));
      if (idStr == NULL) {
         goto fail;
      }

      res = waveOutMessage((HWAVEOUT)IntToPtr(deviceOut),
                           DRV_QUERYFUNCTIONINSTANCEID,
                           (DWORD_PTR)idStr,
                           (DWORD_PTR)idLen);
      if (res != MMSYSERR_NOERROR) {
         CoTaskMemFree(idStr);
         goto fail;
      }

      hr = IMMDeviceEnumerator_GetDevice(denum, idStr, &device);
      if (SUCCEEDED(hr)) {
         hr = IMMDevice_Activate(device,
                                 &IID_IAudioEndpointVolume,
                                 CLSCTX_ALL,
                                 NULL,
                                 &info->outputEP);
         IUnknown_Release(device);
      }

      CoTaskMemFree(idStr);

      if (FAILED(hr)) {
         goto fail;
      }
   }

   if (denum) {
      IUnknown_Release(denum);
   }

   return TRUE;

fail:
   if (denum) {
      IUnknown_Release(denum);
   }

   return cleanup(Px);
}

int open_ep_mixers_byid(px_mixer *Px, wchar_t *deviceIn, wchar_t *deviceOut)
{
   PxEPInfo *info;
   IMMDeviceEnumerator *denum = NULL;
   IMMDevice *device = NULL;
   HRESULT hr;

   if (!initialize(Px)) {
      goto fail;
   }

   info = (PxEPInfo *) Px->info;
   info->inputEP = NULL;
   info->outputEP = NULL;

   // Create an audio endpoint device enumerator.
   hr = CoCreateInstance(&CLSID_MMDeviceEnumerator,
                         NULL,
                         CLSCTX_ALL,
                         &IID_IMMDeviceEnumerator,
                         &denum);
   if (FAILED(hr)) {
      goto fail;
   }

   if (deviceIn) {
      hr = IMMDeviceEnumerator_GetDevice(denum, deviceIn, &device);
      if (SUCCEEDED(hr)) {
         hr = IMMDevice_Activate(device,
                                 &IID_IAudioEndpointVolume,
                                 CLSCTX_ALL,
                                 NULL,
                                 &info->inputEP);
         IUnknown_Release(device);
      }

      if (FAILED(hr)) {
         goto fail;
      }
   }

   if (deviceOut) {
      hr = IMMDeviceEnumerator_GetDevice(denum, deviceOut, &device);
      if (SUCCEEDED(hr)) {
         hr = IMMDevice_Activate(device,
                                 &IID_IAudioEndpointVolume,
                                 CLSCTX_ALL,
                                 NULL,
                                 &info->outputEP);
         IUnknown_Release(device);
      }

      if (FAILED(hr)) {
         goto fail;
      }
   }

   if (denum) {
      IUnknown_Release(denum);
   }

   return TRUE;

fail:
   if (denum) {
      IUnknown_Release(denum);
   }

   return cleanup(Px);
}


static int initialize(px_mixer *Px)
{
   Px->info = calloc(1, sizeof(PxEPInfo));
   if (Px->info == NULL) {
      return FALSE;
   }

   Px->CloseMixer = close_mixer;
//   Px->GetNumMixers = get_num_mixers;
//   Px->GetMixerName = get_mixer_name;
   Px->GetMasterVolume = get_master_volume;
   Px->SetMasterVolume = set_master_volume;
   Px->SupportsPCMOutputVolume = supports_pcm_output_volume;
   Px->GetPCMOutputVolume = get_pcm_output_volume;
   Px->SetPCMOutputVolume = set_pcm_output_volume;
   Px->GetNumOutputVolumes = get_num_output_volumes;
   Px->GetOutputVolumeName = get_output_volume_name;
   Px->GetOutputVolume = get_output_volume;
   Px->SetOutputVolume = set_output_volume;
   Px->GetNumInputSources = get_num_input_sources;
   Px->GetInputSourceName = get_input_source_name;
   Px->GetCurrentInputSource = get_current_input_source;
   Px->SetCurrentInputSource = set_current_input_source;
   Px->GetInputVolume = get_input_volume;
   Px->SetInputVolume = set_input_volume;
   
// Px->SupportsOutputBalance = supports_output_balance;
// Px->GetOutputBalance = get_output_balance;
// Px->SetOutputBalance = set_output_balance;
   
// Px->SupportsPlaythrough = supports_play_through;
// Px->GetPlaythrough = get_play_through;
// Px->SetPlaythrough = set_play_through;
   
   return TRUE;
}

static int cleanup(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   if (info) {
      if (info->outputEP) {
         IUnknown_Release(info->outputEP);
      }

      if (info->inputEP) {
         IUnknown_Release(info->inputEP);
      }

      free(info);
      Px->info = NULL;
   }

   return FALSE;
}

static void close_mixer(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   cleanup(Px);
}

static int get_num_mixers(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   return 0;
}

static const char *get_mixer_name(px_mixer *Px, int i)
{
   return NULL;
}

static PxVolume get_master_volume(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;
   float volume = 0.0;

   if (info->outputEP) {
      IAudioEndpointVolume_GetMasterVolumeLevelScalar(info->outputEP,
                                                      &volume);
   }

   return volume;
}

static void set_master_volume(px_mixer *Px, PxVolume volume)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   if (info->outputEP) {
      IAudioEndpointVolume_SetMasterVolumeLevelScalar(info->outputEP,
                                                      volume, 
                                                      NULL);
   }
   
   return;
}

/*
|| Main output volume
*/

static int supports_pcm_output_volume(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;
   
   return TRUE;
}

static PxVolume get_pcm_output_volume(px_mixer *Px)
{
   return get_master_volume(Px);
}

static void set_pcm_output_volume(px_mixer *Px, PxVolume volume)
{
   set_master_volume(Px, volume);

   return;
}

/*
|| Output info
*/

static int get_num_output_volumes(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   return (info->outputEP ? 1 : 0);
}

static const char *get_output_volume_name(px_mixer *Px, int i)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   return (info->outputEP ? "PCM" : NULL);
}

/*
|| Output volume
*/

static PxVolume get_output_volume(px_mixer *Px, int i)
{
   return get_master_volume(Px);
}

static void set_output_volume(px_mixer *Px, int i, PxVolume volume)
{
   set_master_volume(Px, volume);

   return;
}

/*
|| Input source
*/

static int get_num_input_sources(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   return (info->inputEP ? 1 : 0);
}

static const char *get_input_source_name(px_mixer *Px, int i)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   return (info->inputEP ? "Master" : NULL);
}

static int get_current_input_source(px_mixer *Px)
{
   return 0;
}

static void set_current_input_source(px_mixer *Px, int i)
{
   return;
}

/*
|| Input volume
*/

static PxVolume get_input_volume(px_mixer *Px)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;
   float volume = 0.0;

   if (info->inputEP) { 
      IAudioEndpointVolume_GetMasterVolumeLevelScalar(info->inputEP,
                                                      &volume);
   }

   return volume;
}

static void set_input_volume(px_mixer *Px, PxVolume volume)
{
   PxEPInfo *info = (PxEPInfo *)Px->info;

   if (info->inputEP) {
      IAudioEndpointVolume_SetMasterVolumeLevelScalar(info->inputEP,
                                                      volume, 
                                                      NULL);
   }
   
   return;
}
