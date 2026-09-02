/*
 * PortMixer
 * PortMixer core
 *
 * Copyright (c) 2002, 2006
 *
 * Written by Dominic Mazzoni
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
#include "portaudio.h"
#include "portmixer.h"
#include "px_mixer.h"

#if defined(_WIN32)
#include <windows.h>
#endif

#if defined(__LINUX__) || defined(__APPLE__)
#include <stdlib.h>
#endif
#include <stdlib.h>

#if defined(PX_USE_WIN_MME)
int OpenMixer_Win_MME(px_mixer *Px, int index);
#endif

#if defined(PX_USE_WIN_DSOUND)
int OpenMixer_Win_DirectSound(px_mixer *Px, int index);
#endif

#if defined(PX_USE_WIN_WASAPI)
int OpenMixer_Win_WASAPI(px_mixer *Px, int index);
#endif

#if defined(PX_USE_MAC_COREAUDIO)
int OpenMixer_Mac_CoreAudio(px_mixer *Px, int index);
#endif

#if defined(PX_USE_UNIX_OSS)
int OpenMixer_Unix_OSS(px_mixer *Px, int index);
#endif

#if defined(PX_USE_LINUX_ALSA)
int OpenMixer_Linux_ALSA(px_mixer *Px, int index);
#endif

static px_mixer *verify_mixer(PxMixer *mixer)
{
   px_mixer *Px = (px_mixer *)mixer;

   if (Px == NULL || Px->magic != PX_MIXER_MAGIC)
      return NULL;

   return Px;
}

/*
 Px_OpenMixer() returns a mixer which will work with the given PortAudio
 audio device.  Pass 0 as the index for the first (default) mixer.
*/

PxMixer* Px_OpenMixer(
   PaStream* pa_stream, PaDeviceIndex inputDeviceIndex, PaDeviceIndex outputDeviceIndex, int i
)
{
   if (pa_stream == NULL || (inputDeviceIndex < 0 && outputDeviceIndex < 0))
      return NULL;

   px_mixer *Px;
   int good = TRUE;

   Px = (px_mixer *) malloc(sizeof(px_mixer));
   if (Px == NULL) {
      return NULL;
   }

   Px->magic = PX_MIXER_MAGIC;
   Px->pa_stream = pa_stream;

   Px->input_device_index = inputDeviceIndex;
   Px->output_device_index = outputDeviceIndex;
   
   Px->info = NULL;

   if (!initialize(Px)) {
      free(Px);
      return NULL;
   }

   // inputDeviceIndex and outputDeviceIndex can't be both invalid
   // at the same time.
   const PaDeviceInfo* deviceInfo = Pa_GetDeviceInfo(
      inputDeviceIndex >= 0 ? inputDeviceIndex : outputDeviceIndex);

   // But still we failed
   if (deviceInfo == NULL) {
      free(Px);
      return NULL;
   }

   const PaHostApiInfo* hostApiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);

   if (hostApiInfo == NULL) {
      free(Px);
      return NULL;
   }

   switch (hostApiInfo->type)
   {
#if defined(PX_USE_WIN_MME)
      case paMME:
         good = OpenMixer_Win_MME(Px, i);
      break;
#endif      

#if defined(PX_USE_WIN_DSOUND)
      case paDirectSound:
         good = OpenMixer_Win_DirectSound(Px, i);
      break;
#endif      

#if defined(PX_USE_WIN_WASAPI)
      case paWASAPI:
         good = OpenMixer_Win_WASAPI(Px, i);
      break;
#endif      

#if defined(PX_USE_MAC_COREAUDIO)
      case paCoreAudio:
         good = OpenMixer_Mac_CoreAudio(Px, i);
      break;
#endif

#if defined(PX_USE_UNIX_OSS)
      case paOSS:
         good = OpenMixer_Unix_OSS(Px, i);
      break;
#endif

#if defined(PX_USE_LINUX_ALSA)
      case paALSA:
         good = OpenMixer_Linux_ALSA(Px, i);
      break;
#endif

      default:
         good = FALSE;
      break;
   }

   if (!good) {
      free(Px);
      return NULL;
   }

   return Px;
}

/*
 Px_CloseMixer() closes a mixer opened using Px_OpenMixer and frees any
 memory associated with it. 
*/

void Px_CloseMixer(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->CloseMixer(Px);

   free(Px);

   return;
}

/*
|| Mixer info
*/
int Px_GetNumMixers(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   int cnt;

   if (!Px) {
      return 0;
   }

   cnt = Px->GetNumMixers(Px);

   return cnt;
}

const char *Px_GetMixerName(PxMixer *mixer, int i)
{
   px_mixer *Px = verify_mixer(mixer);
   const char *name;

   if (!Px) {
      return 0;
   }

   name = Px->GetMixerName(Px, i);

   return name;
}

/*
|| Master (output) volume
*/

PxVolume Px_GetMasterVolume(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0.0;

   return Px->GetMasterVolume(Px);
}

void Px_SetMasterVolume(PxMixer *mixer, PxVolume volume)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->SetMasterVolume(Px, volume);

   return;
}

/*
|| Main output volume
*/

PxVolume Px_GetPCMOutputVolume(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0.0;

   return Px->GetPCMOutputVolume(Px);
}

void Px_SetPCMOutputVolume(PxMixer *mixer, PxVolume volume)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->SetPCMOutputVolume(Px, volume);

   return;
}

int Px_SupportsPCMOutputVolume(PxMixer* mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return FALSE;

   return Px->SupportsPCMOutputVolume(Px);
}

/*
|| Output info
*/

int Px_GetNumOutputVolumes(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0;

   return Px->GetNumOutputVolumes(Px);
}

const char *Px_GetOutputVolumeName(PxMixer *mixer, int i)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return NULL;

   return Px->GetOutputVolumeName(Px, i);
}

/*
|| Output volume
*/

PxVolume Px_GetOutputVolume(PxMixer *mixer, int i)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0.0;

   return Px->GetOutputVolume(Px, i);
}

void Px_SetOutputVolume(PxMixer *mixer, int i, PxVolume volume)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->SetOutputVolume(Px, i, volume);

   return;
}

/*
|| Input source
*/

int Px_GetNumInputSources(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0;

   return Px->GetNumInputSources(Px);
}

const char *Px_GetInputSourceName(PxMixer *mixer, int i)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return NULL;

   return Px->GetInputSourceName(Px, i);
}

int Px_GetCurrentInputSource(PxMixer *mixer) /* may return -1 == none */
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return -1;

   return Px->GetCurrentInputSource(Px);
}

void Px_SetCurrentInputSource(PxMixer *mixer, int i)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->SetCurrentInputSource(Px, i);

   return;
}

/*
|| Input volume
*/

PxVolume Px_GetInputVolume(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0.0;

   return Px->GetInputVolume(Px);
}

void Px_SetInputVolume(PxMixer *mixer, PxVolume volume)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->SetInputVolume(Px, volume);

   return;
}

/*
|| Balance
*/

int Px_SupportsOutputBalance(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return FALSE;

   return Px->SupportsOutputBalance(Px);
}

PxBalance Px_GetOutputBalance(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0.0;

   return Px->GetOutputBalance(Px);
}

void Px_SetOutputBalance(PxMixer *mixer, PxBalance balance)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->SetOutputBalance(Px, balance);

   return;
}

/*
|| Playthrough
*/

int Px_SupportsPlaythrough(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return FALSE;

   return Px->SupportsPlaythrough(Px);
}

PxVolume Px_GetPlaythrough(PxMixer *mixer)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return 0.0;

   return Px->GetPlaythrough(Px);
}

void Px_SetPlaythrough(PxMixer *mixer, PxVolume volume)
{
   px_mixer *Px = verify_mixer(mixer);
   if (!Px)
      return;

   Px->SetPlaythrough(Px, volume);

   return;
}

/* ----------------------------------------------------------------------------
|| Default API routines                                                      ||
-----------------------------------------------------------------------------*/

static int initialize(px_mixer *Px)
{
   Px->CloseMixer = close_mixer;
   Px->GetNumMixers = get_num_mixers;
   Px->GetMixerName = get_mixer_name;
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
   Px->SupportsOutputBalance = supports_output_balance;
   Px->GetOutputBalance = get_output_balance;
   Px->SetOutputBalance = set_output_balance;
   Px->SupportsPlaythrough = supports_play_through;
   Px->GetPlaythrough = get_play_through;
   Px->SetPlaythrough = set_play_through;

   return TRUE;
}

static void close_mixer(px_mixer *Px)
{
   return;
}

static int get_num_mixers(px_mixer *Px)
{
   return 0;
}

static const char *get_mixer_name(px_mixer *Px, int i)
{
   return NULL;
}

static PxVolume get_master_volume(px_mixer *Px)
{
   return 0.0;
}

static void set_master_volume(px_mixer *Px, PxVolume volume)
{
   return;
}

/*
|| Main output volume
*/

static int supports_pcm_output_volume(px_mixer *Px)
{
   return FALSE;
}

static PxVolume get_pcm_output_volume(px_mixer *Px)
{
   return 0.0;
}

static void set_pcm_output_volume(px_mixer *Px, PxVolume volume)
{
   return;
}

/*
|| All output volumes
*/

static int get_num_output_volumes(px_mixer *Px)
{
   return 0;
}

static const char *get_output_volume_name(px_mixer *Px, int i)
{
   return NULL;
}

static PxVolume get_output_volume(px_mixer *Px, int i)
{
   return 0.0;
}

static void set_output_volume(px_mixer *Px, int i, PxVolume volume)
{
   return;
}

/*
|| Input source
*/

static int get_num_input_sources(px_mixer *Px)
{
   return 0;
}

static const char *get_input_source_name(px_mixer *Px, int i)
{
   return NULL;
}

static int get_current_input_source(px_mixer *Px)
{
   return -1;
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
   return 0.0;
}

static void set_input_volume(px_mixer *Px, PxVolume volume)
{
   return;
}

/*
|| Balance
*/

static int supports_output_balance(px_mixer *Px)
{
   return FALSE;
}

static PxBalance get_output_balance(px_mixer *Px)
{
   return 0.0;
}

static void set_output_balance(px_mixer *Px, PxBalance balance)
{
   return;
}

/*
|| Playthrough
*/

static int supports_play_through(px_mixer *Px)
{
   return FALSE;
}

static PxVolume get_play_through(px_mixer *Px)
{
   return 0.0;
}

static void set_play_through(px_mixer *Px, PxVolume volume)
{
   return;
}
