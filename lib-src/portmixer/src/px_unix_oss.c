/*
 * PortMixer
 * Unix OSS Implementation
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
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <ctype.h>

#if defined(HAVE_SYS_SOUNDCARD_H)
# include <sys/soundcard.h>
#elif defined(HAVE_LINUX_SOUNDCARD_H)
# include <linux/soundcard.h>
#elif defined(HAVE_MACHINE_SOUNDCARD_H)
# include <machine/soundcard.h> /* JH20010905 */
#else
# error No sound card header file
#endif

#include "portaudio.h"

#include "portmixer.h"
#include "px_mixer.h"

#if !defined(FALSE)
#define FALSE 0
#endif

#if !defined(TRUE)
#define TRUE 1
#endif

#define MIXER_NAME_BASE "/dev/mixer"
#define MIXER_COUNT_MAX 10

typedef struct PxDev
{
   const char *name;
   int fd;
   int num;
   int chans[SOUND_MIXER_NRDEVICES];
} PxDev;

typedef struct PxInfo
{
   int numMixers;
   int mixerIndexes[MIXER_COUNT_MAX];
   char mixers[MIXER_COUNT_MAX][sizeof(MIXER_NAME_BASE) + 1];

   PxDev capture;
   PxDev playback;
} PxInfo;

static const char *labels[] = SOUND_DEVICE_LABELS;

static int open_mixer(PxDev *dev, int cmd)
{
   char name[sizeof(MIXER_NAME_BASE) + 1];
   int mask;
   int i;
   int id = 0;

   for (i = strlen(dev->name) - 1; i >= 0; i++) {
      if (!isdigit(dev->name[i])) {
         break;
      }
   }

   id = atoi(&dev->name[i + 1]);
   if (id < 0 || id >= MIXER_COUNT_MAX) {
      return -1;
   }

   strcpy(name, MIXER_NAME_BASE);
   if (id == 0)
      name[strlen(MIXER_NAME_BASE)] = 0;
   else
      name[strlen(MIXER_NAME_BASE)] = '0' + (id - 1);

   do {
      dev->fd = open(name, O_RDWR);
      if (dev->fd < 0) {
         break;
      }

      if (ioctl(dev->fd, cmd, &mask) == -1) {
         break;
      }

      dev->num = 0;
   
      for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) {
         if (mask & (1 << i)) {
            dev->chans[dev->num++] = i;
         }
      }
      return TRUE;

   } while (FALSE);

   if (dev->fd >= 0) {
      close(dev->fd);
      dev->fd = -1;
   }

   return FALSE;
}

const char* GetDeviceName(PaDeviceIndex device)
{
   const PaDeviceInfo* deviceInfo = Pa_GetDeviceInfo (device);

   if (deviceInfo == NULL)
      return NULL;

   return deviceInfo->name;
}

int OpenMixer_Unix_OSS(px_mixer *Px, int index)
{
   PxInfo *info;

   if (!initialize(Px)) {
      return FALSE;
   }

   get_num_mixers(Px);
   
   info = (PxInfo *) Px->info;
   info->capture.fd = -1;
   info->capture.num = 0;
   info->playback.fd = -1;
   info->playback.num = 0;   
   
   do {
      info->capture.name = GetDeviceName(Px->input_device_index);

      if (info->capture.name) {
         if (!open_mixer(&info->capture, SOUND_MIXER_READ_RECMASK)) {
            break;
         }
      }
      
      info->playback.name = GetDeviceName(Px->output_device_index);
      if (info->playback.name) {
         if (!open_mixer(&info->playback, SOUND_MIXER_READ_DEVMASK)) {
            break;
         }
      }

      return TRUE;

   } while (FALSE);

   return cleanup(Px);
}

static PxVolume get_volume(int fd, int channel)
{
   int vol;
   int stereo;

   if (ioctl(fd, SOUND_MIXER_READ_STEREODEVS, &stereo) == 0)
      stereo = ((stereo & (1 << channel)) != 0);
   else
      stereo = 0;
   
   if (ioctl(fd, MIXER_READ(channel), &vol) == -1)
      return 0.0;

   if (stereo)
      return ((vol & 0xFF)/200.0) + (((vol>>8) & 0xFF)/200.0);
   else
      return (vol & 0xFF)/100.0;
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

   if (info->capture.fd >= 0) {
      close(info->capture.fd);
   }

   if (info->playback.fd >= 0) {
      close(info->playback.fd);
   }

   if (info) {
      free(info);
      Px->info = NULL;
   }
   
   return FALSE;
}

static void close_mixer(px_mixer *Px)
{
   cleanup(Px);

   return;
}

static int get_num_mixers(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   int i;
   int fd;

   info->numMixers = 0;

   for (i = 0; i < MIXER_COUNT_MAX; i++) {
      strcpy(info->mixers[i], MIXER_NAME_BASE);

      if (i == 0)
         info->mixers[i][strlen(MIXER_NAME_BASE)] = 0;
      else
         info->mixers[i][strlen(MIXER_NAME_BASE)] = '0' + (i - 1);

      fd = open(info->mixers[i], O_RDWR);
      if (fd >= 0) {
         info->mixerIndexes[info->numMixers] = i;
         info->numMixers++;
         close(fd);
      }
   }

   return info->numMixers;
}

static const char *get_mixer_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->numMixers <= 0)
      get_num_mixers(Px);

   if (i >= 0 && i < info->numMixers) {
      return info->mixers[info->mixerIndexes[i]];
   }

   return NULL;
}

/*
|| Master volume
*/

static PxVolume get_master_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->playback.fd >= 0) {
      return get_volume(info->playback.fd, SOUND_MIXER_READ_VOLUME);
   }

   return 0.0;
}

static void set_master_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   int vol = (int) ((volume * 100.0) + 0.5);

   if (info->playback.fd >= 0) {
      vol = (vol | (vol << 8));
      ioctl(info->playback.fd, SOUND_MIXER_WRITE_VOLUME, &vol);
   }

   return;
}

/*
|| Main output volume
*/

static int supports_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   int i;

   if (info->playback.fd >= 0) {
      for (i = 0; i < info->playback.num; i++) {
         if (info->playback.chans[i] == SOUND_MIXER_PCM) {
            return TRUE;
         }
      }
   }

   return FALSE;
}

static PxVolume get_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (supports_pcm_output_volume(Px)) {
      return get_volume(info->playback.fd, SOUND_MIXER_READ_PCM);
   }

   return 0.0;
}

static void set_pcm_output_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   int vol = (int) ((volume * 100.0) + 0.5);

   if (supports_pcm_output_volume(Px)) {
      vol = (vol | (vol << 8));
      ioctl(info->playback.fd, SOUND_MIXER_WRITE_PCM, &vol);
   }

   return;
}

/*
|| All output volumes
*/

static int get_num_output_volumes(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return info->playback.num;
}

static const char *get_output_volume_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->playback.fd >= 0) {
      if (i >= 0 && i < info->playback.num) {
         return labels[info->playback.chans[i]];
      }
   }

   return NULL;
}

static PxVolume get_output_volume(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->playback.fd >= 0) {
      if (i >= 0 && i < info->playback.num) {
         return get_volume(info->playback.fd, info->playback.chans[i]);
      }
   }

   return 0.0;
}

static void set_output_volume(px_mixer *Px, int i, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   int vol = (int) ((volume * 100.0) + 0.5);

   if (info->playback.fd >= 0) {
      if (i >= 0 && i < info->playback.num) {
         vol = (vol | (vol << 8));
         ioctl(info->playback.fd, MIXER_WRITE(info->playback.chans[i]), &vol);
      }
   }

   return;
}

/*
|| Input source
*/

static int get_num_input_sources(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return info->capture.num;
}

static const char *get_input_source_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (info->capture.fd >= 0) {
      if (i >= 0 && i < info->capture.num) {
         return labels[info->capture.chans[i]];
      }
   }

   return NULL;
}

static int get_current_input_source(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   int mask;
   int i;

   /* Note that there may be more than one in OSS; we pick
      the first one */
   if (info->capture.fd >= 0) {
      if (ioctl(info->capture.fd, SOUND_MIXER_READ_RECSRC, &mask) == -1) {
         return -1; /* none / error */
      }

      for (i = 0; i < info->capture.num; i++) {
         if (mask & (1 << info->capture.chans[i])) {
            return i;
         }
      }
   }

   return -1; /* none */
}

static void set_current_input_source(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;
   int mask;

   if (info->capture.fd >= 0) {
      if (i >= 0 && i < info->capture.num) {
         mask = (1 << info->capture.chans[i]);
   
         ioctl(info->capture.fd, SOUND_MIXER_WRITE_RECSRC, &mask);
      }
   }

   return;
}

/*
|| Input volume
*/

static PxVolume get_input_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   int i;
   
   if (info->capture.fd >= 0) {
      i = get_current_input_source(Px);
      if (i >= 0) {
         return get_volume(info->capture.fd, info->capture.chans[i]);
      }
   }

   return 0.0;
}

static void set_input_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   int vol;
   int i;

   if (info->capture.fd >= 0) {
      i = get_current_input_source(Px);
      if (i >= 0) {
         vol = (int) ((volume * 100.0) + 0.5);
         vol = (vol | (vol << 8));

         ioctl(info->capture.fd, MIXER_WRITE(info->capture.chans[i]), &vol);
      }
   }

   return;
}
