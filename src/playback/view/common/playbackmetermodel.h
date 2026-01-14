/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "metermodel.h"

#include "playback/iplayback.h"

namespace au::playback {
class PlaybackMeterModel : public MeterModel
{
    Q_OBJECT

    muse::Inject<IPlayback> playback{ this };
    muse::Inject<record::IRecord> record{ this };

public:
    explicit PlaybackMeterModel(QObject* parent = nullptr);

    Q_INVOKABLE void init() override;
};
}
