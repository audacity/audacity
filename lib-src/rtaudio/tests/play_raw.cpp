/******************************************/
/*
  play_raw.c
  by Gary P. Scavone, 2001

  Play a raw file.  It is necessary that the
  file be of the same format as defined below.
  Uses blocking functionality.
*/
/******************************************/

#include "RtAudio.h"
#include <iostream>
#include <stdio.h>

/*
typedef char  MY_TYPE;
#define FORMAT RTAUDIO_SINT8
#define SCALE  127.0

typedef signed short  MY_TYPE;
#define FORMAT RTAUDIO_SINT16
#define SCALE  32767.0

typedef signed long  MY_TYPE;
#define FORMAT RTAUDIO_SINT24
#define SCALE  8388607.0

typedef signed long  MY_TYPE;
#define FORMAT RTAUDIO_SINT32
#define SCALE  2147483647.0
*/

typedef float  MY_TYPE;
#define FORMAT RTAUDIO_FLOAT32
#define SCALE  1.0;

/*
typedef double  MY_TYPE;
#define FORMAT RTAUDIO_FLOAT64
#define SCALE  1.0;
*/

void usage(void) {
  /* Error function in case of incorrect command-line
     argument specifications
  */
  std::cout << "\nuseage: play_raw N fs file <device>\n";
  std::cout << "    where N = number of channels,\n";
  std::cout << "    fs = the sample rate, \n";
  std::cout << "    file = the raw file to play,\n";
  std::cout << "    and device = the device to use (default = 0).\n\n";
  exit(0);
}

int main(int argc, char *argv[])
{
  int chans, fs, buffer_size, count, device = 0;
  long counter = 0;
  MY_TYPE *buffer;
  char *file;
  FILE *fd;
  RtAudio *audio;

  // minimal command-line checking
  if (argc != 4 && argc != 5 ) usage();

  chans = (int) atoi(argv[1]);
  fs = (int) atoi(argv[2]);
  file = argv[3];
  if ( argc == 5 )
    device = (int) atoi(argv[4]);

  fd = fopen(file,"rb");
  if (!fd) {
    std::cout << "can't find file!\n";
    exit(0);
  }

  // Open the realtime output device
  buffer_size = 512;
  try {
    audio = new RtAudio(device, chans, 0, 0,
                        FORMAT, fs, &buffer_size, 2);
  }
  catch (RtError &error) {
    fclose(fd);
    error.printMessage();
    exit(EXIT_FAILURE);
  }

  try {
    buffer = (MY_TYPE *) audio->getStreamBuffer();
    audio->startStream();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  while (1) {
    count = fread(buffer, chans * sizeof(MY_TYPE), buffer_size, fd);

    if (count == buffer_size) {
      try {
        audio->tickStream();
      }
      catch (RtError &error) {
        error.printMessage();
        goto cleanup;
      }
    }
    else
      break;
        
    counter += buffer_size;
  }

  try {
    audio->stopStream();
  }
  catch (RtError &error) {
    error.printMessage();
  }

 cleanup:
  audio->closeStream();
  delete audio;
  fclose(fd);

  return 0;
}
