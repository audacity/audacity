#include <stdio.h>

#include "portmixer.h"
#include "portaudio.h"

static int DummyCallbackFunc(const void *input, void *output,
                             unsigned long frameCount,
                             const PaStreamCallbackTimeInfo* timeInfo,
                             PaStreamCallbackFlags statusFlags, void *userData)
{
   return 0;
}

int main(int argc, char **argv)
{
   int num_mixers;
   int i;
   PaError error;
   PaStream *stream;
   PaStreamParameters inputParameters;
   PaStreamParameters outputParameters;
   int recDeviceNum;
   int playDeviceNum;
   int inputChannels = 2;

   recDeviceNum = Pa_GetDefaultInputDevice();
   playDeviceNum = Pa_GetDefaultOutputDevice();

   inputParameters.device = recDeviceNum;
   inputParameters.channelCount = inputChannels;
   inputParameters.sampleFormat = paFloat32;
   inputParameters.suggestedLatency = 0;
   inputParameters.hostApiSpecificStreamInfo = NULL;

   outputParameters.device = paNoDevice;
   outputParameters.channelCount = 0;
   outputParameters.sampleFormat = paFloat32;
   outputParameters.suggestedLatency = 0;
   outputParameters.hostApiSpecificStreamInfo = NULL;

   error = Pa_OpenStream(&stream, &inputParameters, &outputParameters, 44101,
                         512, paClipOff | paDitherOff, DummyCallbackFunc, NULL);

   if (error) {
      printf("PortAudio error %d: %s\n", error,
             Pa_GetErrorText(error));
      return -1;
   }
   
   num_mixers = Px_GetNumMixers(stream);
   printf("Number of mixers: %d\n", num_mixers);
   for(i=0; i<num_mixers; i++) {
      PxMixer *mixer;
      int num;
      int j;

      printf("Mixer %d: %s\n", i, Px_GetMixerName(stream, i));
      mixer = Px_OpenMixer(stream, i);
      if (!mixer) {
         printf("  Could not open mixer!\n");
         continue;
      }
      
      printf("  Master volume: %.2f\n", Px_GetMasterVolume(mixer));
      printf("  PCM output volume: %.2f\n", Px_GetPCMOutputVolume(mixer));

      num = Px_GetNumOutputVolumes(mixer);
      printf("  Num outputs: %d\n", num);
      for(j=0; j<num; j++) {
         printf("    Output %d (%s): %.2f\n",
                j,
                Px_GetOutputVolumeName(mixer, j),
                Px_GetOutputVolume(mixer, j));
      }

      num = Px_GetNumInputSources(mixer);
      printf("  Num input sources: %d\n", num);
      for(j=0; j<num; j++) {
         printf("    Input %d (%s) %s\n",
                j,
                Px_GetInputSourceName(mixer, j),
                (Px_GetCurrentInputSource(mixer)==j?
                 "SELECTED": ""));
      }
      printf("  Input volume: %.2f\n", Px_GetInputVolume(mixer));

      Px_CloseMixer(mixer);
   }

   Pa_CloseStream(stream);

   return 0;
}
