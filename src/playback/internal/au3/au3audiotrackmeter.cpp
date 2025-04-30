/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "au3audiotrackmeter.h"

#include "libraries/lib-utility/MemoryX.h"

void au::playback::Au3TrackMeter::Update(int64_t trackId, size_t channel, const float* sampleData, unsigned long numFrames)
{
    if (numFrames == 0) {
        return;
    }

    float peak = 0.0f;
    for (size_t i = 0; i < numFrames; ++i) {
        peak = std::max(peak, static_cast<float>(fabs(sampleData[i])));
    }

    m_audioSignalChanges.send(trackId, channel,
                              au::audio::AudioSignalVal { 0, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(peak)) });
}

muse::async::Promise<muse::async::Channel<int64_t, au::audio::audioch_t, au::audio::AudioSignalVal> >
au::playback::Au3TrackMeter::signalChanges() const
{
    return muse::async::Promise<muse::async::Channel<int64_t, au::audio::audioch_t, au::audio::AudioSignalVal> >([this](auto resolve,
                                                                                                                        auto) {
        return resolve(
            m_audioSignalChanges);
    });
}
