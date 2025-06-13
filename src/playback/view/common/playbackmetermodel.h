/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "playback/iplaybackmetercontroller.h"

namespace au::playback {
class PlaybackMeterModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IPlaybackMeterController> meterController;

    Q_PROPERTY(QVariantList smallSteps READ smallSteps NOTIFY smallStepsChanged)
    Q_PROPERTY(QVariantList fullSteps READ fullSteps NOTIFY fullStepsChanged)
    Q_PROPERTY(int meterSize READ meterSize WRITE setMeterSize)

public:
    explicit PlaybackMeterModel(QObject* parent = nullptr);

    Q_INVOKABLE double stepToPosition(double step);
    Q_INVOKABLE double sampleToPosition(double sample) const;
    Q_INVOKABLE QString sampleToText(double sample) const;

    int meterSize() const;
    void setMeterSize(int size);

    QVariantList smallSteps() const;
    QVariantList fullSteps() const;

signals:
    void smallStepsChanged();
    void fullStepsChanged();

private:
    int m_meterSize = 0;
};
}
