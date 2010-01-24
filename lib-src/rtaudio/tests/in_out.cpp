/******************************************/
/*
  in_out.c
  by Gary P. Scavone, 2001

  Records from default input and passes it
  through to the output.  Takes number of
  channels and sample rate as input arguments.
  Use blocking functionality.
*/
/******************************************/

#include "RtAudio.h"
#include <iostream>

/*
typedef signed long  MY_TYPE;
#define FORMAT RTAUDIO_SINT24

typedef char  MY_TYPE;
#define FORMAT RTAUDIO_SINT8

typedef signed short  MY_TYPE;
#define FORMAT RTAUDIO_SINT16

typedef signed long  MY_TYPE;
#define FORMAT RTAUDIO_SINT32

typedef float  MY_TYPE;
#define FORMAT RTAUDIO_FLOAT32
*/

typedef double  MY_TYPE;
#define FORMAT RTAUDIO_FLOAT64

#define TIME   4.0

void usage(void) {
  /* Error function in case of incorrect command-line
     argument specifications
  */
  std::cout << "\nuseage: in_out N fs <device>\n";
  std::cout << "    where N = number of channels,\n";
  std::cout << "    fs = the sample rate,\n";
  std::cout << "    and device = the device to use (default = 0).\n\n";
  exit(0);
}

int main(int argc, char *argv[])
{
  int chans, fs, buffer_size, device = 0;
  long frames, counter = 0;
  MY_TYPE *buffer;
  RtAudio *audio;

  // minimal command-line checking
  if (argc != 3 && argc != 4 ) usage();

  chans = (int) atoi(argv[1]);
  fs = (int) atoi(argv[2]);
  if ( argc == 4 )
    device = (int) atoi(argv[3]);

  // Open the realtime output device
  buffer_size = 512;
  try {
    audio = new RtAudio(device, chans, device, chans,
                        FORMAT, fs, &buffer_size, 8);
  }
  catch (RtError &error) {
    error.printMessage();
    exit(EXIT_FAILURE);
  }

  frames = (long) (fs * TIME);

  try {
    buffer = (MY_TYPE *) audio->getStreamBuffer();
    audio->startStream();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  std::cout << "\nRunning for " << TIME << " seconds ... fragment_size = " << buffer_size << std::endl;
  while (counter < frames) {

    try {
      audio->tickStream();
    }
    catch (RtError &error) {
      error.printMessage();
      goto cleanup;
    }
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

  return 0;
}
