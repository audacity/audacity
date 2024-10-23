/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"
#include "global/async/promise.h"
#include "global/async/channel.h"

#include "au3audio/audiotypes.h"

#include "libraries/lib-audio-devices/Meter.h"

namespace au::playback {
class InOutMeter : public Meter, public muse::async::Asyncable
{
public:
    void Clear() override;
    void Reset(double sampleRate, bool resetClipping) override;
    void UpdateDisplay(unsigned numChannels, unsigned long numFrames, const float* sampleData) override;
    bool IsMeterDisabled() const override;
    float GetMaxPeak() const override;
    bool IsClipping() const override;
    int GetDBRange() const override;

    muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> > signalChanges() const;

private:
    muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> m_audioSignalChanges;
};
}
