/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOListener.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_LISTENER__
#define __AUDACITY_AUDIO_IO_LISTENER__

#include "Audacity.h"

class WaveTrack;
using WaveTrackArray =
   std::vector < std::shared_ptr < WaveTrack > >;

class AUDACITY_DLL_API AudioIOListener /* not final */ {
public:
   AudioIOListener() {}
   virtual ~AudioIOListener() {}

   // Pass 0 when audio stops, positive when it starts:
   virtual void OnAudioIORate(int rate) = 0;

   virtual void OnAudioIOStartRecording() = 0;
   virtual void OnAudioIOStopRecording() = 0;
   virtual void OnAudioIONewBlocks(const WaveTrackArray *tracks) = 0;

   // Commit the addition of temporary recording tracks into the project
   virtual void OnCommitRecording() = 0;

   // During recording, the threshold for sound activation has been crossed
   // in either direction
   virtual void OnSoundActivationThreshold() = 0;

};

#endif
