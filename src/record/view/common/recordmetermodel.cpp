/*
* Audacity: A Digital Audio Editor
*/

#include "recordmetermodel.h"

#include "record/iaudioinput.h"

#include <memory>

using namespace au::record;

RecordMeterModel::RecordMeterModel(QObject* parent)
    : MeterModel(parent)
{
}

void RecordMeterModel::init()
{
    MeterModel::init();

    record()->audioInput()->recordVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume) {
        setVolume(volume);
    });

    record()->audioInput()->recordVolume().onResolve(this, [this](float volume) {
        setVolume(volume);
    });
}
