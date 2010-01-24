#include "config.h"
#ifdef HAVE_PORTAUDIO

#include "play.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

int main(int argc, char **argv)
{
  if(argc<6) {
    printf("usage: sbsmsplay infile.sbsms rate-start[0.2:5] rate-end[0.2:5] halfsteps-start[-12:12] halfsteps-end[-12:12] <quality[0-2](default 2 highest)>\n");
    exit(-1);
  }
  char *filenameIn = argv[1];

  float stretch0 = atof(argv[2]);
  float stretch1 = atof(argv[3]);
  float halfsteps0 = atof(argv[4]);
  float halfsteps1 = atof(argv[5]);
  if(stretch0 < 0.2 || stretch0 > 5) {
    printf("rate-start out of bounds\n");
    exit(-1);
  }
  if(stretch1 < 0.2 || stretch1 > 5) {
    printf("rate-end out of bounds\n");
    exit(-1);
  }
  if(halfsteps0 < -12 || halfsteps0 > 12) {
    printf("halfsteps-start out of bounds\n");
    exit(-1);
  }
  if(halfsteps1 < -12 || halfsteps1 > 12) {
    printf("halfsteps-end out of bounds\n");
    exit(-1);
  }
  
  int quality = 1;
  if(argc == 7) quality = atoi(argv[6]);
  
  real ratio0 = pow(2.0,-halfsteps0/12.0);
  real ratio1 = pow(2.0,-halfsteps1/12.0);

  sbsmsplayer *player = new sbsmsplayer(filenameIn,stretch0,stretch1,ratio0,ratio1);
  player->play();
  while(player->isPlaying())
    Pa_Sleep(100);
  player->close();
  delete player;
}

#endif
