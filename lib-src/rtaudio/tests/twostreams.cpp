/******************************************/
/*
  twostreams.cpp
  by Gary P. Scavone, 2001

  Test executable for audio playback, recording,
  duplex operation, stopping, starting, and
  aborting operations.  Takes number of channels
  and sample rate as input arguments.  Runs input
  and output through two separate instances of RtAudio.
  Uses blocking functionality.
*/
/******************************************/

#include "RtAudio.h"
#include <iostream>

/*
typedef signed long  MY_TYPE;
#define FORMAT RTAUDIO_SINT24
#define SCALE  2147483647.0

typedef char  MY_TYPE;
#define FORMAT RTAUDIO_SINT8
#define SCALE  127.0

typedef signed short  MY_TYPE;
#define FORMAT RTAUDIO_SINT16
#define SCALE  32767.0

typedef signed long  MY_TYPE;
#define FORMAT RTAUDIO_SINT32
#define SCALE  2147483647.0
*/

typedef float  MY_TYPE;
#define FORMAT RTAUDIO_FLOAT32
#define SCALE  1.0

/*
typedef double  MY_TYPE;
#define FORMAT RTAUDIO_FLOAT64
#define SCALE  1.0
*/

#define BASE_RATE 0.005
#define TIME 2.0

void usage(void) {
  /* Error function in case of incorrect command-line
     argument specifications
  */
  std::cout << "\nuseage: twostreams N fs <device>\n";
  std::cout << "    where N = number of channels,\n";
  std::cout << "    fs = the sample rate,\n";
  std::cout << "    and device = the device to use (default = 0).\n\n";
  exit(0);
}

int main(int argc, char *argv[])
{
  int chans, fs, buffer_size, device = 0;
  long frames, counter = 0, i, j;
  MY_TYPE *buffer1, *buffer2;
  RtAudio *stream1, *stream2;
  FILE *fd;
  double *data = 0;

  // minimal command-line checking
  if (argc != 3 && argc != 4 ) usage();

  chans = (int) atoi(argv[1]);
  fs = (int) atoi(argv[2]);
  if ( argc == 4 )
    device = (int) atoi(argv[3]);

  // Open the realtime output device
  buffer_size = 512;
  try {
    stream1 = new RtAudio(device, chans, 0, 0,
                          FORMAT, fs, &buffer_size, 8);
  }
  catch (RtError &error) {
    error.printMessage();
    exit(EXIT_FAILURE);
  }

  try {
    stream2 = new RtAudio(0, 0, device, chans,
                          FORMAT, fs, &buffer_size, 8);
  }
  catch (RtError &error) {
    delete stream1;
    error.printMessage();
    exit(EXIT_FAILURE);
  }

  try {
    buffer1 = (MY_TYPE *) stream1->getStreamBuffer();
    buffer2 = (MY_TYPE *) stream2->getStreamBuffer();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  frames = (long) (fs * TIME);
  data = (double *) calloc(chans, sizeof(double));

  try {
    stream1->startStream();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  std::cout << "\nStarting sawtooth playback stream for " << TIME << " seconds." << std::endl;
  while (counter < frames) {
    for (i=0; i<buffer_size; i++) {
      for (j=0; j<chans; j++) {
        buffer1[i*chans+j] = (MY_TYPE) (data[j] * SCALE);
        data[j] += BASE_RATE * (j+1+(j*0.1));
        if (data[j] >= 1.0) data[j] -= 2.0;
      }
    }

    try {
      stream1->tickStream();
    }
    catch (RtError &error) {
      error.printMessage();
      goto cleanup;
    }

    counter += buffer_size;
  }

  std::cout << "\nStopping playback stream." << std::endl;
  try {
    stream1->stopStream();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  fd = fopen("test.raw","wb");

  try {
    stream2->startStream();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  counter = 0;
  std::cout << "\nStarting recording stream for " << TIME << " seconds." << std::endl;
  while (counter < frames) {

    try {
      stream2->tickStream();
    }
    catch (RtError &error) {
      error.printMessage();
      goto cleanup;
    }

    fwrite(buffer2, sizeof(MY_TYPE), chans * buffer_size, fd);
    counter += buffer_size;
  }

  fclose(fd);
  std::cout << "\nAborting recording." << std::endl;

  try {
    stream2->abortStream();
    stream1->startStream();
    stream2->startStream();
  }
  catch (RtError &error) {
    error.printMessage();
    goto cleanup;
  }

  counter = 0;
  std::cout << "\nStarting playback and record streams (quasi-duplex) for " << TIME << " seconds." << std::endl;
  while (counter < frames) {

    try {
      stream2->tickStream();
      memcpy(buffer1, buffer2, sizeof(MY_TYPE) * chans * buffer_size);
      stream1->tickStream();
    }
    catch (RtError &error) {
      error.printMessage();
      goto cleanup;
    }

    counter += buffer_size;
  }

  std::cout << "\nStopping both streams." << std::endl;
  try {
    stream1->stopStream();
    stream2->stopStream();
  }
  catch (RtError &error) {
    error.printMessage();
  }

 cleanup:
  stream1->closeStream();
  stream2->closeStream();
  delete stream1;
  delete stream2;
  if (data) free(data);

  return 0;
}
