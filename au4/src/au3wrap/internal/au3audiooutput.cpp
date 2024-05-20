/*
* Audacity: A Digital Audio Editor
*/

#include "au3audiooutput.h"

#include "global/async/async.h"

#include "playback/audiotypes.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-audio-devices/Meter.h"

#include "log.h"

using namespace muse;
using namespace muse::async;
using namespace au::au3;

class InOutMeter : public Meter, public muse::async::Asyncable
{
public:
    void Clear() override
    {
    }

    void Reset(double sampleRate, bool resetClipping) override
    {
        UNUSED(sampleRate);
        UNUSED(resetClipping);
    }

    void UpdateDisplay(unsigned numChannels, unsigned long numFrames, const float* sampleData) override
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
                peak[j] = std::max(peak[j], fabs(sptr[j]));
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

        m_audioSignalChanges.send(0, au::audio::AudioSignalVal { 0, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(peak[0])) });
        m_audioSignalChanges.send(1, au::audio::AudioSignalVal { 0, static_cast<au::audio::volume_dbfs_t>(LINEAR_TO_DB(peak[1])) });
        // LOGD() << "=============== change " << LINEAR_TO_DB(peak[0]) << " - " << LINEAR_TO_DB(peak[1]);
    }

    bool IsMeterDisabled() const override
    {
        NOT_IMPLEMENTED;
        return false;
    }

    float GetMaxPeak() const override
    {
        NOT_IMPLEMENTED;
        return 0.0;
    }

    bool IsClipping() const override
    {
        NOT_IMPLEMENTED;
        return false;
    }

    int GetDBRange() const override
    {
        NOT_IMPLEMENTED;
        return 0;
    }

    muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> > signalChanges() const
    {
        return muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> >([this](auto resolve, auto /*reject*/) {
            return resolve(
                m_audioSignalChanges);
        });
    }

private:
    muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> m_audioSignalChanges;
};

Au3AudioOutput::Au3AudioOutput()
{
    m_outputMeter = std::make_shared<InOutMeter>();

    globalContext()->currentProjectChanged().onNotify(this, [this](){
        auto currentProject = globalContext()->currentProject();
        if (!currentProject) {
            return;
        }

        initMeter();
    });
}

void Au3AudioOutput::initMeter()
{
    AudacityProject& project = projectRef();

    auto& projectAudioIO = ProjectAudioIO::Get(project);
    projectAudioIO.SetPlaybackMeter(m_outputMeter);
}

muse::async::Promise<float> Au3AudioOutput::playbackVolume() const
{
    return muse::async::Promise<float>([](auto resolve, auto /*reject*/) {
        float inputVolume;
        float outputVolume;
        int inputSource;

        auto gAudioIO = AudioIO::Get();
        gAudioIO->GetMixer(&inputSource, &inputVolume, &outputVolume);

        return resolve(outputVolume);
    });
}

void Au3AudioOutput::setPlaybackVolume(float volume)
{
    muse::async::Async::call(this, [this, volume]() {
        float inputVolume;
        float outputVolume;
        int inputSource;

        auto gAudioIO = AudioIO::Get();
        gAudioIO->GetMixer(&inputSource, &inputVolume, &outputVolume);

        gAudioIO->SetMixer(inputSource, inputVolume, volume);

        m_playbackVolumeChanged.send(volume);
    });
}

muse::async::Channel<float> Au3AudioOutput::playbackVolumeChanged() const
{
    return m_playbackVolumeChanged;
}

muse::async::Promise<muse::async::Channel<au::audio::audioch_t, au::audio::AudioSignalVal> > Au3AudioOutput::playbackSignalChanges() const
{
    return m_outputMeter->signalChanges();
}

AudacityProject& Au3AudioOutput::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
