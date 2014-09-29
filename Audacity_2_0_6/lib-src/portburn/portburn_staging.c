/*
 * PortBurn
 * Common utilities for staging audio files in a temporary directory.
 *
 * Dominic Mazzoni
 * License: LGPL
 *
 * A library for cross-platform audio CD burning
 *
 * On some of the platforms supported by PortBurn, we need to stage
 * the audio data ourselves.  This file defines an interface to the
 * common functions.
 */

#include "portburn_staging.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>

#define swap_uint16(x) \
       ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8))
#define swap_uint32(x) \
       ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |    \
        (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))

inline const unsigned int FourCharInt32(const char *str) {
   return swap_uint32(*(const unsigned int *)(str));
}

typedef struct {
   int      ntracks;
   int      cur;
   FILE    *curfp;
   char    *tmpdir;
   char    *filename[99];
   char    *trackname[99];
   int      frames[99];
   int      needswap;
} StagingHandle;

static void PortBurn_Put32(StagingHandle *handle, unsigned int data)
{
   if (handle->needswap)
      data = swap_uint32(data);
   fwrite((void *)&data, 4, 1, handle->curfp);
}

static void PortBurn_Put16(StagingHandle *handle, unsigned short data)
{
   if (handle->needswap)
      data = swap_uint16(data);
   fwrite((void *)&data, 2, 1, handle->curfp);
}

void *PortBurn_TempDirStaging(const char *temporary_directory) {
   StagingHandle *h;
   volatile unsigned int swaptest;
   FILE *fp;
   char *tmp2;
   char *tmp;
   int len;
   int i;

   if (temporary_directory == NULL)
      return NULL;

   len = strlen(temporary_directory);
   if (len < 2)
      return NULL;

   tmp = (char *)malloc(len+1);
   tmp2 = (char *)malloc(len+6);
   strcpy(tmp, temporary_directory);

   /* Make all of the parent directories, in case they don't exist. */
   for(i=1; i<len; i++) {
      if (tmp[i]=='/') {
         tmp[i] = 0;
         mkdir(tmp, 0777);
         tmp[i] = '/';
      }
   }
   mkdir(tmp, 0777);

   /* The path shouldn't end in a '/' */
   if (tmp[len-1] == '/') {
      tmp[len-1] = 0;
      len--;
   }

   /* Make sure we can write and read files in this directory */
   sprintf(tmp2, "%s/test", tmp);
   fp = fopen(tmp2, "w");
   if (!fp) {
      free(tmp);
      free(tmp2);
      return NULL;
   }
   fprintf(fp, "test");
   fclose(fp);
   fp = fopen(tmp2, "r");
   if (!fp) {
      unlink(tmp2);
      free(tmp);
      free(tmp2);
      return NULL;
   }
   fclose(fp);
   unlink(tmp2);

   h = (StagingHandle *)malloc(sizeof(StagingHandle));
   memset(h, 0, sizeof(StagingHandle));
   h->tmpdir = tmp;
   free(tmp2);

   swaptest = 0x01020304;
   if (((unsigned char *)&swaptest)[0] == 1)
      h->needswap = 1;
   else
      h->needswap = 0;

   h->cur = -1;

   return h;
}

int PortBurn_StartStagingTrack(void *handle, const char *name, int frames)
{
   StagingHandle *h = (StagingHandle *)handle;
   if (!h)
      return -1;

   if (h->cur != -1)
      return -1;

   if (name == NULL)
      return -1;

   if (h->ntracks == 99)
      return -1;

   h->cur = h->ntracks;
   h->trackname[h->cur] = (char *)malloc(strlen(name)+1);
   strcpy(h->trackname[h->cur], name);
   h->filename[h->cur] = (char *)malloc(strlen(h->tmpdir)+14);
   sprintf(h->filename[h->cur], "%s/track%03d.wav", h->tmpdir, h->cur+1);
   h->curfp = fopen(h->filename[h->cur], "wb");
   if (!h->curfp) {
      free(h->trackname[h->cur]);
      free(h->filename[h->cur]);
      h->cur = -1;
      return -1;
   }
   h->frames[h->cur] = frames;

   struct WavFormatChunk {
      unsigned short format;
      unsigned short channels;
      unsigned int samplerate;
      unsigned int byterate;
      unsigned short blockAlign;
      unsigned short bitsPerSample;
   };

   PortBurn_Put32(h, FourCharInt32("RIFF"));
   PortBurn_Put32(h, 44 + (4 * 1176 * frames));
   PortBurn_Put32(h, FourCharInt32("WAVE"));

   PortBurn_Put32(h, FourCharInt32("fmt "));
   PortBurn_Put32(h, 16);
   PortBurn_Put16(h, 1);       /* format */
   PortBurn_Put16(h, 2);       /* channels */
   PortBurn_Put32(h, 44100);   /* samplerate */
   PortBurn_Put32(h, 176400);  /* byterate */
   PortBurn_Put16(h, 4);       /* block align */
   PortBurn_Put16(h, 16);      /* bits per sample */

   PortBurn_Put32(h, FourCharInt32("data"));
   PortBurn_Put32(h, 4 * 1176 * frames);

   if (ferror(h->curfp)) {
      /* Finish this track and let the cleanup function handle it */
      fclose(h->curfp);
      h->ntracks++;
      h->cur = -1;
      return -1;
   }

   return 0;
}

int PortBurn_AddStagingFrame(void *handle, short *buffer)
{
   StagingHandle *h = (StagingHandle *)handle;
   int i;

   if (!h)
      return -1;

   if (h->cur != h->ntracks)
      return -1;

   if (!buffer)
      return -1;

   for(i = 0; i < 1176; i++) {
      PortBurn_Put16(h, buffer[i]);
   }

   if (ferror(h->curfp)) {
      /* Finish this track and let the cleanup function handle it */
      fclose(h->curfp);
      h->ntracks++;
      h->cur = -1;
      return -1;
   }

   return 0;
}

int PortBurn_EndStagingTrack(void *handle)
{
   StagingHandle *h = (StagingHandle *)handle;
   if (!h)
      return -1;

   if (h->cur != h->ntracks)
      return -1;

   fclose(h->curfp);
   h->ntracks++;
   h->cur = -1;

   return 0;
}

int PortBurn_FinishStaging(void *handle)
{
   return 0;
}

int PortBurn_GetNumStagedTracks(void *handle)
{
   StagingHandle *h = (StagingHandle *)handle;
   if (!h)
      return -1;

   return h->ntracks;
}

const char *PortBurn_GetStagedFilename(void *handle, int index)
{
   StagingHandle *h = (StagingHandle *)handle;
   if (!h)
      return NULL;

   return h->filename[index];
}

const char *PortBurn_GetStagedTrackName(void *handle, int index)
{
   StagingHandle *h = (StagingHandle *)handle;
   if (!h)
      return NULL;

   return h->trackname[index];
}

int PortBurn_GetStagedLengthInFrames(void *handle, int index)
{
   StagingHandle *h = (StagingHandle *)handle;
   if (!h)
      return -1;

   return h->frames[index];
}

void PortBurn_CleanupStaging(void *handle)
{
   StagingHandle *h = (StagingHandle *)handle;
   int i;

   if (!h)
      return;

   free(h->tmpdir);
   for(i = 0; i < h->ntracks; i++) {
      unlink(h->filename[i]);
      free(h->filename[i]);
      free(h->trackname[i]);
   }
}
