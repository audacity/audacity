/*
    mp3tech.c - Functions for handling MP3 files and most MP3 data
                structure manipulation.

    Copyright (C) 2000-2006 Cedric Tefft <cedric@phreaker.net>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  ***************************************************************************

  This file is based in part on:

	* MP3Info 0.5 by Ricardo Cerqueira <rmc@rccn.net>
	* MP3Stat 0.9 by Ed Sweetman <safemode@voicenet.com> and 
			 Johannes Overmann <overmann@iname.com>

*/

#include "mp3tech.h"
#include <sys/stat.h>
#include <string.h>
#include <ctype.h>

namespace _sbsms_ {
int layer_tab[4]= {0, 3, 2, 1};

int frequencies[3][4] = {
   {22050,24000,16000,50000},  /* MPEG 2.0 */
   {44100,48000,32000,50000},  /* MPEG 1.0 */
   {11025,12000,8000,50000}    /* MPEG 2.5 */
};

int bitrate[2][3][15] = { 
  { /* MPEG 2.0 */
    {0,32,48,56,64,80,96,112,128,144,160,176,192,224,256},  /* layer 1 */
    {0,8,16,24,32,40,48,56,64,80,96,112,128,144,160},       /* layer 2 */
    {0,8,16,24,32,40,48,56,64,80,96,112,128,144,160}        /* layer 3 */

  },

  { /* MPEG 1.0 */
    {0,32,64,96,128,160,192,224,256,288,320,352,384,416,448}, /* layer 1 */
    {0,32,48,56,64,80,96,112,128,160,192,224,256,320,384},    /* layer 2 */
    {0,32,40,48,56,64,80,96,112,128,160,192,224,256,320}      /* layer 3 */
  }
};

int frame_samples_index[] = {384,1152,1152};

int frame_size_index[] = {24000, 72000, 72000};


char *mode_text[] = {
   "stereo", "joint stereo", "dual channel", "mono"
};

char *emphasis_text[] = {
  "none", "50/15 microsecs", "reserved", "CCITT J 17"
};


int get_mp3_info(mp3info *mp3)
{
  int had_error = 0;
  int frame_type[15]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  int samples = 0;
  int bitrate;
  int frames=0,frame_types=0,frames_so_far=0;
  int counter=0;
  mp3header header;
  struct stat filestat;
  off_t data_start=0;

  int scantype = SCAN_FULL;

  stat(mp3->filename,&filestat);
  mp3->datasize=filestat.st_size;
  get_id3(mp3);

  if(scantype == SCAN_FULL) {
    if(get_first_header(mp3,0L)) {
      data_start=ftell(mp3->file);
      while((bitrate=get_next_header(mp3))) {
	frame_type[15-bitrate]++;
	frames++;
      }
      memcpy(&header,&(mp3->header),sizeof(mp3header));
      for(counter=0;counter<15;counter++) {
	if(frame_type[counter]) {
	  frame_types++;
	  header.bitrate=counter;
	  frames_so_far += frame_type[counter];
	  samples += frame_samples(&mp3->header)*frame_type[counter];
	  mp3->sample_rate = header_frequency(&mp3->header);
	}
      }
      mp3->samples = samples - frame_samples_index[2];
    }
  }
 
  return had_error;
}


int get_first_header(mp3info *mp3, long startpos) 
{
  int k, l=0,c;
  mp3header h, h2;
  long valid_start=0;
  
  fseek(mp3->file,startpos,SEEK_SET);
  while (1) {
    while((c=fgetc(mp3->file)) != 255 && (c != EOF));
    if(c == 255) {
      ungetc(c,mp3->file);
      valid_start=ftell(mp3->file);
      if((l=get_header(mp3->file,&h))) {
	fseek(mp3->file,l-FRAME_HEADER_SIZE,SEEK_CUR);
	for(k=1; (k < MIN_CONSEC_GOOD_FRAMES) && (mp3->datasize-ftell(mp3->file) >= FRAME_HEADER_SIZE); k++) {
	  if(!(l=get_header(mp3->file,&h2))) break;
	  if(!sameConstant(&h,&h2)) break;
	  fseek(mp3->file,l-FRAME_HEADER_SIZE,SEEK_CUR);
	}
	if(k == MIN_CONSEC_GOOD_FRAMES) {
	  fseek(mp3->file,valid_start,SEEK_SET);
	  memcpy(&(mp3->header),&h2,sizeof(mp3header));
	  mp3->header_isvalid=1;
	  return 1;
	} 
      }
    } else {
      return 0;
    }
  }
  
  return 0;  
}

/* get_next_header() - read header at current position or look for 
   the next valid header if there isn't one at the current position
*/
int get_next_header(mp3info *mp3) 
{
  int l=0,c,skip_bytes=0;
  mp3header h;
  
  while(1) {
    while((c=fgetc(mp3->file)) != 255 && (ftell(mp3->file) < mp3->datasize)) skip_bytes++;
    if(c == 255) {
      ungetc(c,mp3->file);
      if((l=get_header(mp3->file,&h))) {
	fseek(mp3->file,l-FRAME_HEADER_SIZE,SEEK_CUR);
	return 15-h.bitrate;
      } else {
	skip_bytes += FRAME_HEADER_SIZE;
      }
    } else {
      return 0;
    }
  }
}

/* Get next MP3 frame header.
   Return codes:
   positive value = Frame Length of this header
   0 = No, we did not retrieve a valid frame header
*/

int get_header(FILE *file,mp3header *header)
{
  unsigned char buffer[FRAME_HEADER_SIZE];
  int fl;
  
  if(fread(&buffer,FRAME_HEADER_SIZE,1,file)<1) {
    header->sync=0;
    return 0;
  }
  header->sync=(((int)buffer[0]<<4) | ((int)(buffer[1]&0xE0)>>4));
  if(buffer[1] & 0x10) header->version=(buffer[1] >> 3) & 1;
  else header->version=2;
  header->layer=(buffer[1] >> 1) & 3;
  header->bitrate=(buffer[2] >> 4) & 0x0F;
  if((header->sync != 0xFFE) || (header->layer != 1) || (header->bitrate == 0xF)) {
    header->sync=0;
    return 0;
  }
  header->crc=buffer[1] & 1;
  header->freq=(buffer[2] >> 2) & 0x3;
  header->padding=(buffer[2] >>1) & 0x1;
  header->extension=(buffer[2]) & 0x1;
  header->mode=(buffer[3] >> 6) & 0x3;
  header->mode_extension=(buffer[3] >> 4) & 0x3;
  header->copyright=(buffer[3] >> 3) & 0x1;
  header->original=(buffer[3] >> 2) & 0x1;
  header->emphasis=(buffer[3]) & 0x3;
  
  /* Final sanity checks: bitrate 1111b and frequency 11b are reserved (invalid) */
  if (header->bitrate == 0x0F || header->freq == 0x3) {
    return 0;
  }
  
  return ((fl=frame_length(header)) >= MIN_FRAME_SIZE ? fl : 0); 
}

int frame_samples(mp3header *header) {
  return frame_samples_index[3-header->layer];
}

int frame_length(mp3header *header) {
  return header->sync == 0xFFE ? 
    (frame_size_index[3-header->layer]*((header->version&1)+1)*
     header_bitrate(header)/header_frequency(header))+
    header->padding : 1;
}

int header_layer(mp3header *h) {return layer_tab[h->layer];}

int header_bitrate(mp3header *h) {
  return bitrate[h->version & 1][3-h->layer][h->bitrate];
}

int header_frequency(mp3header *h) {
  return frequencies[h->version][h->freq];
}

char *header_emphasis(mp3header *h) {
  return emphasis_text[h->emphasis];
}

char *header_mode(mp3header *h) {
  return mode_text[h->mode];
}

int sameConstant(mp3header *h1, mp3header *h2) {
  if((*(uint*)h1) == (*(uint*)h2)) return 1;
  
  if((h1->version       == h2->version         ) &&
     (h1->layer         == h2->layer           ) &&
     (h1->crc           == h2->crc             ) &&
     (h1->freq          == h2->freq            ) &&
     (h1->mode          == h2->mode            ) &&
     (h1->copyright     == h2->copyright       ) &&
     (h1->original      == h2->original        ) &&
     (h1->emphasis      == h2->emphasis        )) 
    return 1;
  else return 0;
}


int get_id3(mp3info *mp3) {
  int retcode=0;
  char fbuf[4];
  
  if(mp3->datasize >= 128) {
    if(fseek(mp3->file, -128, SEEK_END )) {
      fprintf(stderr,"ERROR: Couldn't read last 128 bytes of %s!!\n",mp3->filename);
      retcode |= 4;
    } else {
      fread(fbuf,1,3,mp3->file); fbuf[3] = '\0';
      mp3->id3.genre[0]=255;
      
      if (!strcmp((const char *)"TAG",(const char *)fbuf)) {
	mp3->id3_isvalid=1;
	mp3->datasize -= 128;
	fseek(mp3->file, -125, SEEK_END);
	fread(mp3->id3.title,1,30,mp3->file); mp3->id3.title[30] = '\0';
	fread(mp3->id3.artist,1,30,mp3->file); mp3->id3.artist[30] = '\0';
	fread(mp3->id3.album,1,30,mp3->file); mp3->id3.album[30] = '\0';
	fread(mp3->id3.year,1,4,mp3->file); mp3->id3.year[4] = '\0';
	fread(mp3->id3.comment,1,30,mp3->file); mp3->id3.comment[30] = '\0';
	if(mp3->id3.comment[28] == '\0') {
	  mp3->id3.track[0] = mp3->id3.comment[29];
	}
	fread(mp3->id3.genre,1,1,mp3->file);
	unpad(mp3->id3.title);
	unpad(mp3->id3.artist);
	unpad(mp3->id3.album);
	unpad(mp3->id3.year);
	unpad(mp3->id3.comment);
      }
    }
  }
  return retcode;
}

char *unpad(char *string) {
  char *pos=string+strlen(string)-1;
  while(isspace(pos[0])) (pos--)[0]=0;
  return string;
}

}