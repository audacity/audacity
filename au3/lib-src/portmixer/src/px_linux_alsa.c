/*
 * PortMixer
 * Linux ALSA Implementation
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

#include <stdio.h>
#include <stdlib.h>
#include <alsa/asoundlib.h>

#include "portaudio.h"
#include "pa_linux_alsa.h"

#include "portmixer.h"
#include "px_mixer.h"

#if !defined(FALSE)
#define FALSE 0
#endif

#if !defined(TRUE)
#define TRUE 1
#endif

typedef struct PxSelem
{
   snd_mixer_elem_t *elem;
   snd_mixer_elem_t *vol;
   unsigned int index;
   unsigned int item;
   char *name;
} PxSelem;

typedef struct PxDev
{
   snd_mixer_t *handle;
   int card;
   int playback;
   int source;

   int numselems;
   PxSelem *selems;
} PxDev;

typedef struct PxInfo
{
   int numMixers;
   char *mixers;

   PxDev playback;
   PxDev capture;
} PxInfo;

static int open_mixer(PxDev *dev, int card, int playback)
{
   do {
      snd_mixer_selem_id_t *sid;
      snd_mixer_elem_t *elem;
      char name[256];
      int err;
      int i;
   
      snd_mixer_selem_id_alloca(&sid);
   
      sprintf(name, "hw:%d", card);
   
      dev->card = card;
      dev->handle = NULL;
      dev->playback = playback;
      dev->source = -1;
   
      err = snd_mixer_open(&dev->handle, 0);
      if (err < 0) {
         break;
      }

      err = snd_mixer_attach(dev->handle, name);
      if (err < 0) {
         break;
      }

      err = snd_mixer_selem_register(dev->handle, NULL, NULL);
      if (err < 0) {
         break;
      }

      err = snd_mixer_load(dev->handle);
      if (err < 0) {
         break;
      }

      for (elem = snd_mixer_first_elem(dev->handle);
           elem != NULL;
           elem = snd_mixer_elem_next(elem))
      {
         if (playback) {
            if (snd_mixer_selem_has_common_volume(elem) ||
                snd_mixer_selem_has_playback_volume(elem)) {
               dev->numselems++;
            }
         }
         else {
            if (snd_mixer_selem_get_capture_group(elem) >= 0) {
               dev->numselems++;
            }
            else if (snd_mixer_selem_is_enum_capture(elem)) {
               int retval = snd_mixer_selem_get_enum_items(elem);

               if (retval > 0)
                  dev->numselems += retval;
            }
         }
      }

      dev->selems = calloc(dev->numselems, sizeof(PxSelem));
      if (dev->selems == NULL) {
         break;
      }


      i = 0;
      for (elem = snd_mixer_first_elem(dev->handle);
           elem != NULL;
           elem = snd_mixer_elem_next(elem))
      {
         snd_mixer_elem_t *vol;
         int ndx;

         if (playback) {
            if (snd_mixer_selem_has_common_volume(elem) ||
                snd_mixer_selem_has_playback_volume(elem)) {
               snprintf(name,
                        sizeof(name),
                        "%s:%d",
                        snd_mixer_selem_get_name(elem),
                        snd_mixer_selem_get_index(elem));
      
               dev->selems[i].elem = elem;
               dev->selems[i].index = snd_mixer_selem_get_index(elem);
               dev->selems[i].name = strdup(name);
               if (!dev->selems[i].name) {
                  break;
               }
               i++;
            }
            continue;
         }

         snd_mixer_selem_id_set_name(sid, "Capture");
         snd_mixer_selem_id_set_index(sid, snd_mixer_selem_get_index(elem));
         vol = snd_mixer_find_selem(dev->handle, sid);

         if (snd_mixer_selem_get_capture_group(elem) >= 0) {
            snprintf(name,
                     sizeof(name),
                     "%s:%d",
                     snd_mixer_selem_get_name(elem),
                     snd_mixer_selem_get_index(elem));

            dev->selems[i].vol = vol;
            dev->selems[i].elem = elem;
            dev->selems[i].index = snd_mixer_selem_get_index(elem);
            dev->selems[i].name = strdup(name);
            if (!dev->selems[i].name) {
               break;
            }
            i++;
         }
         else if (snd_mixer_selem_is_enum_capture(elem)) {
            int j;
            int cnt = snd_mixer_selem_get_enum_items(elem);

            if (cnt < 0)
               continue;

            for (j = 0; j < cnt; j++) {
               char iname[256];
               snd_mixer_selem_get_enum_item_name(elem, (unsigned int) j, sizeof(iname), iname);
               snprintf(name,
                        sizeof(name),
                        "%s:%d",
                        iname,
                        snd_mixer_selem_get_index(elem));

               dev->selems[i].vol = vol;
               dev->selems[i].item = j;
               dev->selems[i].elem = elem;
               dev->selems[i].index = snd_mixer_selem_get_index(elem);
               dev->selems[i].name = strdup(name);
               if (!dev->selems[i].name) {
                  break;
               }
               i++;
            }
         }
      }

      if (i != dev->numselems) {
         break;
      }

      if (playback) {
         return TRUE;
      }

      for (i = 0; i < dev->numselems; i++) {
         elem = dev->selems[i].elem;
         if (snd_mixer_selem_get_capture_group(elem) >= 0) {
            int sw = 0;
            if (snd_mixer_selem_get_capture_switch(elem, SND_MIXER_SCHN_FRONT_LEFT, &sw) < 0) {
               continue;
            }
            if (!sw) {
               continue;
            }
            dev->source = i;
            break;
         }
         else if (snd_mixer_selem_is_enum_capture(elem)) {
            unsigned int src;
            if (snd_mixer_selem_get_enum_item(elem, SND_MIXER_SCHN_FRONT_LEFT, &src) < 0) {
               continue;
            }
            if (src == dev->selems[i].item) {
               dev->source = i;
               break;
            }
         }
      }

      if (dev->source == -1) {
         dev->source = 0;
      }

      return TRUE;

   } while (FALSE);

   if (dev->selems) {
      int i;
      for (i = 0; i < dev->numselems; i++) {
         if (dev->selems[i].name) {
            free(dev->selems[i].name);
         }
      }
      free(dev->selems);
      dev->selems = NULL;
   }

   if (dev->handle) {
      snd_mixer_close(dev->handle);
      dev->handle = NULL;
   }

   return FALSE;
}

int OpenMixer_Linux_ALSA(px_mixer *Px, int index)
{
   PxInfo *info;
   int card;

   if (!initialize(Px)) {
      return FALSE;
   }

   info = (PxInfo *) Px->info;

   if (PaAlsa_GetStreamInputCard(Px->pa_stream, &card) == paNoError) {
      if (!open_mixer(&info->capture, card, FALSE)) {
         return cleanup(Px);
      }
   }

   if (PaAlsa_GetStreamOutputCard(Px->pa_stream, &card) == paNoError) {
      if (!open_mixer(&info->playback, card, TRUE)) {
         return cleanup(Px);
      }
   }

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
   
//   Px->SupportsOutputBalance = supports_output_balance;
//   Px->GetOutputBalance = get_output_balance;
//   Px->SetOutputBalance = set_output_balance; 
//   Px->SupportsPlaythrough = supports_play_through;
//   Px->GetPlaythrough = get_play_through;
//   Px->SetPlaythrough = set_play_through;
   
   return TRUE;
}

static int cleanup(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   int i;

   if (info->capture.selems) {
      for (i = 0; i < info->capture.numselems; i++) {
         if (info->capture.selems[i].name) {
            free(info->capture.selems[i].name);
         }
      }
      free(info->capture.selems);
   }

   if (info->capture.handle) {
      snd_mixer_close(info->capture.handle);
   }

   if (info->playback.selems) {
      for (i = 0; i < info->playback.numselems; i++) {
         if (info->playback.selems[i].name) {
            free(info->playback.selems[i].name);
         }
      }
      free(info->playback.selems);
   }

  if (info->playback.handle) {
      snd_mixer_close(info->playback.handle);
   }

   if (info) {
      free(info);
      Px->info = NULL;
   }

   return FALSE;
}

static int generic_lookup(PxDev *dev, const char *generic)
{
   snd_mixer_selem_id_t *id;
   snd_mixer_elem_t *elem;
   int i;

   if (dev == NULL) {
      return -1;
   }

   for (i = 0; i < dev->numselems; i++) {
      if (strncmp(dev->selems[i].name, generic, strlen(generic)) == 0) {
         return i;
      }
   }

   return -1;
}

static PxVolume get_volume_indexed(PxDev *dev, int i)
{
   snd_mixer_elem_t *elem;
   long vol, min, max;

   if (!dev->handle) {
      return 0.0;
   }

   if (i < 0 || i > dev->numselems) {
      return 0.0;
   }

   elem = dev->selems[i].elem;
   if (dev->playback) {
      snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
      if (snd_mixer_selem_has_playback_channel(elem, SND_MIXER_SCHN_FRONT_LEFT)) {
         snd_mixer_selem_get_playback_volume(elem, SND_MIXER_SCHN_FRONT_LEFT, &vol);
         return (PxVolume) vol / (max - min);
      }
   }
   else {
      snd_mixer_selem_get_capture_volume_range(elem, &min, &max);
      if (snd_mixer_selem_has_capture_channel(elem, SND_MIXER_SCHN_FRONT_LEFT)) {
         snd_mixer_selem_get_capture_volume(elem, SND_MIXER_SCHN_FRONT_LEFT, &vol);
         return (PxVolume) vol / (max - min);
      }
   }
   
   return 0.0;
}

static PxVolume get_volume(PxDev *dev, const char *name)
{
   int i;

   if (!dev->handle) {
      return 0.0;
   }

   for (i = 0; i < dev->numselems; i++) {
      if (strcmp(dev->selems[i].name, name) == 0) {
         return get_volume_indexed(dev, i);
      }
   }

   return 0.0;
}

static void set_volume_indexed(PxDev *dev, int i, PxVolume volume)
{
   snd_mixer_elem_t *elem;
   long vol, min, max;
   int j;

   if (!dev->handle) {
      return;
   }

   if (i < 0 || i > dev->numselems) {
      return;
   }

   elem = dev->selems[i].elem;
   if (dev->playback) {
      snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
      for (j = 0; j < SND_MIXER_SCHN_LAST; j++) {
         if (snd_mixer_selem_has_playback_channel(elem, j)) {
            vol = (long) (volume * (max - min) + 0.5);
            snd_mixer_selem_set_playback_volume(elem, j, vol);
         }
      }
   }
   else {
      snd_mixer_selem_get_capture_volume_range(elem, &min, &max);
      for (j = 0; j < SND_MIXER_SCHN_LAST; j++) {
         if (snd_mixer_selem_has_capture_channel(elem, j)) {
            vol = (long) (volume * (max - min) + 0.5);
            snd_mixer_selem_set_capture_volume(elem, j, vol);
         }
      }
   }

   return;
}

static void set_volume(PxDev *dev, const char *name, PxVolume volume)
{
   int i;

   if (!dev->handle) {
      return;
   }

   for (i = 0; i < dev->numselems; i++) {
      if (strcmp(dev->selems[i].name, name) == 0) {
         set_volume_indexed(dev, i, volume);
         break;
      }
   }

   return;
}

static void close_mixer(px_mixer *Px)
{
   cleanup(Px);

   return;
}

static int get_num_mixers(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return 1;
}

static const char *get_mixer_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   return "ALSA";
}

/*
|| Master volume
*/

static PxVolume get_master_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   PxDev *dev = &info->playback;

   return get_volume_indexed(dev, generic_lookup(dev, "Master"));
}

static void set_master_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   PxDev *dev = &info->playback;

   set_volume_indexed(dev, generic_lookup(dev, "Master"), volume);

   /* Ensure pending events are handled...otherwise, they build up */
   if (dev->handle)
      snd_mixer_handle_events(dev->handle);

   return;
}

/* 
|| Main output volume
*/

static int supports_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   PxDev *dev = &info->playback;

   return generic_lookup(dev, "PCM") != -1;
}

static PxVolume get_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   PxDev *dev = &info->playback;

   return get_volume_indexed(dev, generic_lookup(dev, "PCM"));
}

static void set_pcm_output_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   PxDev *dev = &info->playback;

   set_volume_indexed(dev, generic_lookup(dev, "PCM"), volume);

   /* Ensure pending events are handled...otherwise, they build up */
   if (dev->handle)
      snd_mixer_handle_events(dev->handle);

   return;
}

/*
|| All output volumes
*/

static int get_num_output_volumes(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->playback.handle) {
      return info->playback.numselems;
   }

   return 0;
}

static const char *get_output_volume_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->playback.handle) {
      if (i >= 0 && i < info->playback.numselems) {
         return info->playback.selems[i].name;
      }
   }

   return NULL;
}

static PxVolume get_output_volume(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   return get_volume_indexed(&info->playback, i);
}

static void set_output_volume(px_mixer *Px, int i, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   set_volume_indexed(&info->playback, i, volume);

   return;
}

/*
|| Input source
*/

static int get_num_input_sources(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->capture.handle) {
      return info->capture.numselems;
   }

   return 0;
}

static const char *get_input_source_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->capture.handle) {
      if (i >= 0 && i < info->capture.numselems) {
         return info->capture.selems[i].name;
      }
   }

   return NULL;
}

static int get_current_input_source(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   snd_mixer_elem_t *elem;
   int i;

   if (!info->capture.handle) {
      return -1;
   }

   return info->capture.source;
}

static void set_current_input_source(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;
   snd_mixer_elem_t *elem;

   if (!info->capture.handle) {
      return;
   }

   if (i < 0 || i >= info->capture.numselems) {
      return;
   }

   elem = info->capture.selems[i].elem;
   if (snd_mixer_selem_get_capture_group(elem) >= 0) {
      snd_mixer_selem_set_capture_switch_all(elem, TRUE);
   }
   else if (snd_mixer_selem_is_enum_capture(elem)) {
      int j;
      for (j = 0; j < SND_MIXER_SCHN_LAST; j++) {
         snd_mixer_selem_set_enum_item(elem, j, info->capture.selems[i].item);
      }
   }

   info->capture.source = i;

   /* Ensure pending events are handled...otherwise, they build up */
   snd_mixer_handle_events(info->capture.handle);

   set_input_volume(Px, get_input_volume(Px));

   return;
}

/*
|| Input volume
*/

static PxVolume get_input_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   snd_mixer_elem_t *elem;
   long min;
   long max;
   long vol;
   PxVolume volume = 0.0;

   if (info->capture.source < 0 || info->capture.numselems < 1) {
      return volume;
   }

   elem = info->capture.selems[info->capture.source].vol;
   if (!elem) {
      return volume;
   }

   snd_mixer_selem_get_capture_volume_range(elem, &min, &max);
   if (snd_mixer_selem_has_capture_channel(elem, SND_MIXER_SCHN_FRONT_LEFT)) {
      snd_mixer_selem_get_capture_volume(elem, SND_MIXER_SCHN_FRONT_LEFT, &vol);
      volume = ((PxVolume) vol) / (max - min);
   }

   return volume;
}

static void set_input_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   snd_mixer_elem_t *elem;
   long min;
   long max;
   long vol;

   if (info->capture.source < 0 || info->capture.numselems < 1) {
      return;
   }

   elem = info->capture.selems[info->capture.source].vol;
   if (!elem) {
      return;
   }

   snd_mixer_selem_get_capture_volume_range(elem, &min, &max);
   vol = (long) (volume * (max - min) + 0.5);
   snd_mixer_selem_set_capture_volume_all(elem, vol);

   if (snd_mixer_selem_has_capture_switch(elem)) {
      snd_mixer_selem_set_capture_switch_all(elem, vol > 0 ? TRUE : FALSE);
   }

   /* Ensure pending events are handled...otherwise, they build up */
   snd_mixer_handle_events(info->capture.handle);

   return;
}
