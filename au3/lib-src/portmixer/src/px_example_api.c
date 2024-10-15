/*
 * PortMixer
 * Example API Implementation
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

typedef struct PxInfo
{
   int         example;
} PxInfo;

int OpenMixer_Platform_API(px_mixer *Px, int index)
{
   PxInfo *info;

   if (!initialize(Px)) {
      return FALSE;
   }
   
   info = (PxInfo *) Px->info;

   return TRUE;
}

static int initialize(px_mixer *Px)
{
   Px->info = calloc(1, sizeof(PxInfo));
   if (Px->info == NULL) {
      return FALSE;
   }

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

static int cleanup(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info) {
      free(info);
      Px->info = NULL;
   }

   return FALSE;
}

static void close_mixer(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}

static int get_num_mixers(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0;
}

static const char *get_mixer_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   return NULL;
}

/*
|| Master volume
*/

static PxVolume get_master_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0.0;
}

static void set_master_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}

/*
|| Main output volume
*/

static int supports_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return FALSE;
}

static PxVolume get_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0.0;
}

static void set_pcm_output_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}

/*
|| All output volumes
*/

static int get_num_output_volumes(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0;
}

static const char *get_output_volume_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   return NULL;
}

static PxVolume get_output_volume(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0.0;
}

static void set_output_volume(px_mixer *Px, int i, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}

/*
|| Input source
*/

static int get_num_input_sources(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0;
}

static const char *get_input_source_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   return NULL;
}

static int get_current_input_source(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return -1;
}

static void set_current_input_source(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}

/*
|| Input volume
*/

static PxVolume get_input_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0.0;
}

static void set_input_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}

/*
|| Balance
*/

static int supports_output_balance(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return FALSE;
}

static PxBalance get_output_balance(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0.0;
}

static void set_output_balance(px_mixer *Px, PxBalance balance)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}

/*
|| Playthrough
*/

static int supports_play_through(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return FALSE;
}

static PxVolume get_play_through(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 0.0;
}

void set_play_through(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   return;
}
