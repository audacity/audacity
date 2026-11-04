/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QtQml/qqmlregistration.h>

#include "framework/global/modularity/ioc.h"

#include "playback/view/common/metermodel.h"
#include "record/irecordconfiguration.h"

namespace au::record {
class SoundActivationLevelModel : public playback::MeterModel
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(double level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    muse::GlobalInject<IRecordConfiguration> recordConfiguration;

public:
    explicit SoundActivationLevelModel(QObject* parent = nullptr);

    Q_INVOKABLE void init() override;
    Q_INVOKABLE void apply();

    double level() const;
    void setLevel(double level);

signals:
    void levelChanged();

private:
    double m_level = 0.0;
};
}
