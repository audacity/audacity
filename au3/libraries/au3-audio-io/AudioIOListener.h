/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOListener.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/
#ifndef __AUDACITY_AUDIO_IO_LISTENER__
#define __AUDACITY_AUDIO_IO_LISTENER__

#include <memory>
#include <vector>

class AUDIO_IO_API AudioIOListener /* not final */
{
public:
    AudioIOListener() {}
    virtual ~AudioIOListener();

    // Pass 0 when audio stops, positive when it starts:
    virtual void OnAudioIORate(int rate) = 0;

    virtual void OnAudioIOStartRecording() = 0;
    virtual void OnAudioIOStopRecording() = 0;
    virtual void OnAudioIONewBlocks() = 0;

    // Commit the addition of temporary recording tracks into the project
    virtual void OnCommitRecording() = 0;

    // AU4: deferred capture — the recording was committed while the stream
    // keeps running (capture disarmed during playback). Default-bodied so
    // existing listeners are unaffected.
    virtual void OnAudioIOCaptureStopped() {}

    // During recording, the threshold for sound activation has been crossed
    // in either direction
    virtual void OnSoundActivationThreshold() = 0;
};

#endif
