/*
* Audacity: A Digital Audio Editor
*/

#include "audacity3audiooutput.h"

#include "types/ret.h"
#include "global/async/async.h"

#include "libraries/lib-audio-io/AudioIO.h"

#include "log.h"

using namespace muse;
using namespace muse::async;
using namespace au::au3;

muse::async::Promise<au::audio::volume_dbfs_t> Audacity3AudioOutput::playbackVolume() const
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

void Audacity3AudioOutput::setPlaybackVolume(float volume)
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

muse::async::Channel<au::audio::volume_dbfs_t> Audacity3AudioOutput::playbackVolumeChanged() const
{
    return m_playbackVolumeChanged;
}

muse::async::Promise<au::audio::AudioSignalChanges> Audacity3AudioOutput::playbackSignalChanges() const
{
    return muse::async::Promise<audio::AudioSignalChanges>([](auto, auto reject) {
        muse::Ret ret = make_ret(muse::Ret::Code::NotImplemented);
        return reject(ret.code(), ret.text());
    });
}
