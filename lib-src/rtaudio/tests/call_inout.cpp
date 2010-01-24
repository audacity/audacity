/******************************************/
/*
  call_inout.cpp
  by Gary P. Scavone, 2001

  Records from default input and passes it
  through to the output.  Takes number of
  channels and sample rate as input arguments.
  Uses callback functionality.
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

void usage(void) {
  /* Error function in case of incorrect command-line
     argument specifications
  */
  std::cout << "\nuseage: call_inout N fs device\n";
  std::cout << "    where N = number of channels,\n";
  std::cout << "    fs = the sample rate,\n";
  std::cout << "    and device = the device to use (default = 0).\n\n";
  exit(0);
}

int inout(char *buffer, int buffer_size, void *)
{
  // Surprise!!  We do nothing to pass the data through.
  return 0;
}

int main(int argc, char *argv[])
{
  int chans, fs, device = 0;
  RtAudio *audio;
  char input;

  // minimal command-line checking
  if (argc != 3 && argc != 4 ) usage();

  chans = (int) atoi(argv[1]);
  fs = (int) atoi(argv[2]);
  if ( argc == 4 )
    device = (int) atoi(argv[3]);

  // Open the realtime output device
  int buffer_size = 512;
  try {
    audio = new RtAudio(device, chans, device, chans,
                        FORMAT, fs, &buffer_size, 8);
  }
  catch (RtError &error) {
    error.printMessage();
    exit(EXIT_FAILURE);
  }

  try {
    audio->setStreamCallback(&inout, NULL);
    audio->startStream();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  std::cout << "\nRunning ... press <enter> to quit (buffer size = " << buffer_size << ").\n";
  std::cin.get(input);

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
