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
#define swap_uint32z(x) \
       ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |    \
        (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))

inline unsigned int FourCharInt32(const char *str) {
   unsigned char *s = (unsigned char *)str;
   return s[0] << 24 |
          s[1] << 16 |
          s[2] << 8 |
          s[3];
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
   if (handle->needswap) {
      data = swap_uint32(data);
   }

   fwrite((void *)&data, 4, 1, handle->curfp);
}

static void PortBurn_Put16(StagingHandle *handle, unsigned short data)
{
   if (handle->needswap) {
      data = swap_uint16(data);
   }

   fwrite((void *)&data, 2, 1, handle->curfp);
}

void *PortBurn_TempDirStaging(const char *temporary_directory)
{
   StagingHandle *h;
   volatile unsigned int swaptest;
   FILE *fp;
   char *tmp2;
   char *tmp;
   int len;
   int i;

   if (temporary_directory == NULL) {
      return NULL;
   }

   len = strlen(temporary_directory);
   if (len < 2) {
      return NULL;
   }

   tmp = (char *) malloc(len + 1);
   if (tmp == NULL) {
      return NULL;
   }
   strcpy(tmp, temporary_directory);

   tmp2 = (char *) malloc(len + 6);
   if (tmp2 == NULL) {
      free(tmp);
      return NULL;
   }

   /* Make all of the parent directories, in case they don't exist. */
   for (i = 1; i < len; i++) {
      if (tmp[i] == '/') {
         tmp[i] = 0;
         mkdir(tmp, 0777);
         tmp[i] = '/';
      }
   }
   mkdir(tmp, 0777);

   /* The path shouldn't end in a '/' */
   if (tmp[len - 1] == '/') {
      tmp[len - 1] = 0;
      len--;
   }

   /* Make sure we can write and read files in this directory */
   sprintf(tmp2, "%s/test", tmp);
   fp = fopen(tmp2, "w");
   if (fp == NULL) {
      free(tmp2);
      free(tmp);
      return NULL;
   }
   fprintf(fp, "test");
   fclose(fp);

   fp = fopen(tmp2, "r");
   if (fp == NULL) {
      unlink(tmp2);
      free(tmp2);
      free(tmp);
      return NULL;
   }
   fclose(fp);
   unlink(tmp2);
   free(tmp2);

   h = (StagingHandle *) calloc(1, sizeof(StagingHandle));
   h->tmpdir = tmp;

   swaptest = 0x01020304;
   if (((unsigned char *)&swaptest)[0] == 1) {
      h->needswap = 0;
   }
   else {
      h->needswap = 1;
   }

   h->cur = -1;

   return h;
}

int PortBurn_StartStagingTrack(void *handle, const char *name, int raw)
{
   StagingHandle *h = (StagingHandle *)handle;

   if (h == NULL) {
      return -1;
   }

   if (h->cur != -1) {
      return -1;
   }

   if (name == NULL) {
      return -1;
   }

   if (h->ntracks == 99) {
      return -1;
   }

   h->cur = h->ntracks;

   h->trackname[h->cur] = (char *) malloc(strlen(name) + 1);
   if (h->trackname[h->cur] == NULL) {
      return -1;
   }
   strcpy(h->trackname[h->cur], name);

   h->filename[h->cur] = (char *) malloc(strlen(h->tmpdir) + 14);
   if (h->filename[h->cur] == NULL) {
      free(h->trackname[h->cur]);
      h->trackname[h->cur] = NULL;
      h->cur = -1;
      return -1;
   }
   sprintf(h->filename[h->cur], "%s/track%03d.au", h->tmpdir, h->cur + 1);

   h->curfp = fopen(h->filename[h->cur], "wb");
   if (h->curfp == NULL) {
      free(h->filename[h->cur]);
      h->filename[h->cur] = NULL;
      free(h->trackname[h->cur]);
      h->trackname[h->cur] = NULL;
      h->cur = -1;
      return -1;
   }
   h->frames[h->cur] = 0;

   PortBurn_Put32(h, FourCharInt32(".snd")); /* magic */
   PortBurn_Put32(h, 24);                    /* header size */
   PortBurn_Put32(h, 0xffffffff);            /* length of data (optional) */
   PortBurn_Put32(h, 3);                     /* 16-bit linear PCM */
   PortBurn_Put32(h, 44100);                 /* samples per second */
   PortBurn_Put32(h, 2);                     /* number of channels */
   
   if (ferror(h->curfp)) {
      /* Finish this track and let the cleanup function handle it */
      PortBurn_EndStagingTrack(h);
      return -1;
   }

   return 0;
}

int PortBurn_AddStagingFrame(void *handle, short *buffer)
{
   StagingHandle *h = (StagingHandle *)handle;
   int i;

   if (h == NULL) {
      return -1;
   }

   if (h->cur != h->ntracks) {
      return -1;
   }

   if (buffer == NULL) {
      return -1;
   }

   for (i = 0; i < 1176; i++) {
      PortBurn_Put16(h, buffer[i]);
   }

   if (ferror(h->curfp)) {
      /* Finish this track and let the cleanup function handle it */
      PortBurn_EndStagingTrack(h);
      return -1;
   }

   return 0;
}

int PortBurn_EndStagingTrack(void *handle)
{
   StagingHandle *h = (StagingHandle *)handle;

   if (h == NULL) {
      return -1;
   }

   if (h->cur != h->ntracks) {
      return -1;
   }

   fclose(h->curfp);
   h->curfp = NULL;
   h->ntracks++;
   h->cur = -1;

   return 0;
}

int PortBurn_FinishStaging(void *handle)
{
   StagingHandle *h = (StagingHandle *)handle;

   if (h == NULL) {
      return -1;
   }

   return 0;
}

int PortBurn_GetNumStagedTracks(void *handle)
{
   StagingHandle *h = (StagingHandle *)handle;

   if (h == NULL) {
      return -1;
   }

   return h->ntracks;
}

const char *PortBurn_GetStagedFilename(void *handle, int index)
{
   StagingHandle *h = (StagingHandle *)handle;

   if (h == NULL) {
      return NULL;
   }

   if (index < 0 || index >= h->ntracks) {
      return NULL;
   }

   return h->filename[index];
}

const char *PortBurn_GetStagedTrackName(void *handle, int index)
{
   StagingHandle *h = (StagingHandle *)handle;

   if (h == NULL) {
      return NULL;
   }

   if (index < 0 || index >= h->ntracks) {
      return NULL;
   }

   return h->trackname[index];
}

int PortBurn_GetStagedLengthInFrames(void *handle, int index)
{
   StagingHandle *h = (StagingHandle *)handle;

   if (h == NULL) {
      return -1;
   }

   if (index < 0 || index >= h->ntracks) {
      return -1;
   }

   return h->frames[index];
}

void PortBurn_CleanupStaging(void *handle)
{
   StagingHandle *h = (StagingHandle *)handle;
   int i;

   if (h == NULL) {
      return;
   }

   if (h->curfp != NULL) {
      fclose(h->curfp);
   }

   if (h->tmpdir) {
      free(h->tmpdir);
   }

   for (i = 0; i < h->ntracks; i++) {
      if (h->filename[i] != NULL) {
         unlink(h->filename[i]);
         free(h->filename[i]);
      }

      if (h->trackname[i] != NULL) {
         free(h->trackname[i]);
      }
   }

   free(h);
}
