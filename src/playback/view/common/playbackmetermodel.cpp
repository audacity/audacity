/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmetermodel.h"

#include "playback/iaudiooutput.h"
#include "record/iaudioinput.h"

#include <memory>

using namespace au::playback;

PlaybackMeterModel::PlaybackMeterModel(QObject* parent)
    : MeterModel(parent)
{
}

void PlaybackMeterModel::init()
{
    MeterModel::init();

    playback()->audioOutput()->playbackVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume) {
        setVolume(volume);
    });

    playback()->audioOutput()->playbackVolume().onResolve(this, [this](float volume) {
        setVolume(volume);
    });
}
