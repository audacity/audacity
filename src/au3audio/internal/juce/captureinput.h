/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "audio/iaudiometer.h"

#include "au3-mixer/AudioIOSequences.h"

namespace au::au4juce {
//! The input of a device, as captured: with the input volume applied, metered, and played through, as AudioIO does
class CaptureInput
{
public:
    //! @param maxFrames the most frames read at once
    CaptureInput(int numChannels, int maxFrames);

    int numChannels() const { return m_numChannels; }

    //! Device thread: reads `frames` frames of `input`, one buffer per channel, with `volume` applied. Returns them
    //! interleaved.
    const float* read(const float* const* input, int frames, float volume);

    //! Device thread: sends what was last read to `meter`, for each of `sequences` it is recorded into, and as a whole
    void sendToMeter(audio::IAudioMeter& meter, const RecordableSequences& sequences, audio::TimePoint time);

    //! Device thread: adds what was last read to `output`, from `offset` on; a mono input goes to the first two
    //! channels
    void playThrough(float* const* output, int numOutputChannels, int offset) const;

private:
    const int m_numChannels;
    std::vector<float> m_interleaved;
    std::vector<float> m_mainMeter;
    int m_frames = 0;
};
}
