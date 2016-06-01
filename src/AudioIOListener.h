/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOListener.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_LISTENER__
#define __AUDACITY_AUDIO_IO_LISTENER__

#include <wx/string.h>

class AutoSaveFile;

class AUDACITY_DLL_API AudioIOListener /* not final */ {
public:
   AudioIOListener() {}
   virtual ~AudioIOListener() {}

   // Pass 0 when audio stops, positive when it starts:
   virtual void OnAudioIORate(int rate) = 0;

   virtual void OnAudioIOStartRecording() = 0;
   virtual void OnAudioIOStopRecording() = 0;
   virtual void OnAudioIONewBlockFiles(const AutoSaveFile & blockFileLog) = 0;
};

#endif
