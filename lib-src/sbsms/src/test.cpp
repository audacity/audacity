#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "convert.h"

#include <unistd.h>

bool progressCB(int percent, const char *msg, void *data) {
  static int lastPercent = 0;
  if (lastPercent != percent) {
    printf("\r%s: %3i%% [%-40s] ", msg, percent>100?100:percent, &"||||||||||||||||||||||||||||||||||||||||"[40 - ((percent>100)?40:(2*percent/5))] );
    lastPercent = percent;
    fflush(stdout);
  }
  return true;
}

int main(int argc, char **argv)
{
  if(argc<7) {
    printf("usage: sbsms infile<.wav|.aif|.mp3|.sbsms> outfile<.wav|.aif|.mp3|.sbsms> rate-start[0.2:5] rate-end[0.2:5] halfsteps-start[-12:12] halfsteps-end[-12:12] <quality[0-2](default 1) <0:1 for pre-analysis>\n");
    exit(-1);
  }
  float srOut = 44100.0;

  bool bAnalyzeOnly = false;
  bool bSynthesizeOnly = false;
  char *filenameIn = argv[1];
  char *filenameOut = argv[2];
  
  if(strstr(filenameIn,".sbsms")) {
    bSynthesizeOnly = true;
  }
  if(strstr(filenameOut,".sbsms")) {
    bAnalyzeOnly = true;
  }
  if(bAnalyzeOnly && bSynthesizeOnly) {
    printf("Can't convert from .sbsms to .sbsms format\n");
    exit(-1);
  }
  
  float stretch0 = atof(argv[3]);
  float stretch1 = atof(argv[4]);
  float halfsteps0 = atof(argv[5]);
  float halfsteps1 = atof(argv[6]);
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
  if(argc >= 8) quality = atoi(argv[7]);

  bool bPreAnalyze = false;
  if(argc >= 9) bPreAnalyze = atoi(argv[8])?true:false;
  
  real ratio0 = pow(2.0,-halfsteps0/12.0);
  real ratio1 = pow(2.0,-halfsteps1/12.0);

  sbsms_init(4096);
  sbsms_convert(filenameIn, filenameOut, bAnalyzeOnly, bSynthesizeOnly, bPreAnalyze, quality, progressCB, NULL, stretch0, stretch1, ratio0, ratio1, 1.0f);
  printf("\n");
}
