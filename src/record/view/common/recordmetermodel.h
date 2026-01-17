/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "playback/view/common/metermodel.h"
#include "record/irecord.h"

namespace au::record {
class RecordMeterModel : public playback::MeterModel
{
    Q_OBJECT

    muse::Inject<IRecord> record{ this };

public:
    explicit RecordMeterModel(QObject* parent = nullptr);

    Q_INVOKABLE void init() override;
};
}
