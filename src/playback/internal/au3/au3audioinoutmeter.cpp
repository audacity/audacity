/*
* Audacity: A Digital Audio Editor
*/

#include <algorithm>
#include <cmath>

#include "au3audioinoutmeter.h"

#include "libraries/lib-utility/MemoryX.h"

#include "log.h"

using namespace muse;
using namespace muse::async;

void au::playback::InOutMeter::Clear()
{
}

void au::playback::InOutMeter::Reset(double sampleRate, bool resetClipping)
{
    UNUSED(sampleRate);
    UNUSED(resetClipping);

    au::audio::volume_dbfs_t zero = static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(0));

    m_audioSignalChanges.send(0, au::audio::AudioSignalVal { 0, zero });
    m_audioSignalChanges.send(1, au::audio::AudioSignalVal { 0, zero });
}

void au::playback::InOutMeter::UpdateDisplay(unsigned int numChannels, unsigned long numFrames, const float* sampleData)
{
    auto sptr = sampleData;
    unsigned int num = 2;

    int numPeakSamplesToClip = 3;

    std::vector<float> peak(num);
    std::vector<float> rms(num);
    std::vector<bool> clipping(num);
    std::vector<unsigned long> headPeakCount(num);
    std::vector<int> tailPeakCount(num);

    for (unsigned long i = 0; i < numFrames; i++) {
        for (unsigned int j = 0; j < num; j++) {
            peak[j] = std::max(peak[j], static_cast<float>(fabs(sptr[j])));
            rms[j] += sptr[j] * sptr[j];

            // In addition to looking for mNumPeakSamplesToClip peaked
            // samples in a row, also send the number of peaked samples
            // at the head and tail, in case there's a run of peaked samples
            // that crosses block boundaries
            if (fabs(sptr[j]) >= MAX_AUDIO) {
                if (headPeakCount[j] == i) {
                    headPeakCount[j]++;
                }
                tailPeakCount[j]++;
                if (tailPeakCount[j] > numPeakSamplesToClip) {
                    clipping[j] = true;
                }
            } else {
                tailPeakCount[j] = 0;
            }
        }
        sptr += numChannels;
    }
    for (unsigned int j = 0; j < num; j++) {
        rms[j] = sqrt(rms[j] / numFrames);
    }
    //std::cout << "UpdateDisplay peak: " << peak[0] << " - " << peak[1] << std::endl;

    m_audioSignalChanges.send(0, au::audio::AudioSignalVal { 0, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(peak[0])) });
    m_audioSignalChanges.send(1, au::audio::AudioSignalVal { 0, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(peak[1])) });
    // LOGD() << "=============== change " << LINEAR_TO_DB(peak[0]) << " - " << LINEAR_TO_DB(peak[1]);
}

void au::playback::InOutMeter::UpdateDisplayPerTrack(const float* sampleData, int64_t trackId, unsigned channel, size_t len)
{
    if (!len) {
        return;
    }

    float peak = 0.0f;
    for (size_t i = 0; i < len; ++i) {
        peak = std::max(peak, static_cast<float>(fabs(sampleData[i])));
    }

    //std::cout << "UpdateDisplayPerTrack peak: " << channel << " - " << peak << std::endl;
    m_audioSignalChangesByTrack.send(trackId, channel,
                                     au::audio::AudioSignalVal { 0, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(peak)) });
}

bool au::playback::InOutMeter::IsMeterDisabled() const
{
    //NOT_IMPLEMENTED;
    return false;
}

float au::playback::InOutMeter::GetMaxPeak() const
{
    NOT_IMPLEMENTED;
    return 0.0;
}

bool au::playback::InOutMeter::IsClipping() const
{
    NOT_IMPLEMENTED;
    return false;
}

int au::playback::InOutMeter::GetDBRange() const
{
    NOT_IMPLEMENTED;
    return 0;
}

muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> >
au::playback::InOutMeter::signalChanges() const
{
    return muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> >([this](auto resolve, auto /*reject*/) {
        return resolve(
            m_audioSignalChanges);
    });
}

muse::async::Promise<muse::async::Channel<uint8_t, au::audio::audioch_t, au::audio::AudioSignalVal> >
au::playback::InOutMeter::signalChangesByTrack() const
{
    return muse::async::Promise<muse::async::Channel<uint8_t, au::audio::audioch_t, au::audio::AudioSignalVal> >([this](auto resolve, auto /*reject*/) {
        return resolve(
            m_audioSignalChangesByTrack);
    });
}
