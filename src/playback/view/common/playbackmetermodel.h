/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackmetercontroller.h"

namespace au::playback {
class PlaybackMeterModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QVariantList smallSteps READ smallSteps NOTIFY smallStepsChanged)
    Q_PROPERTY(QVariantList fullSteps READ fullSteps NOTIFY fullStepsChanged)

    Q_PROPERTY(PlaybackMeterStyle::MeterStyle meterStyle READ meterStyle WRITE setMeterStyle NOTIFY meterStyleChanged FINAL)
    Q_PROPERTY(PlaybackMeterType::MeterType meterType READ meterType WRITE setMeterType NOTIFY meterTypeChanged FINAL)
    Q_PROPERTY(
        PlaybackMeterPosition::MeterPosition meterPosition READ meterPosition WRITE setMeterPosition NOTIFY meterPositionChanged FINAL)
    Q_PROPERTY(
        PlaybackMeterDbRange::DbRange meterDbRange READ meterDbRange WRITE setMeterDbRange NOTIFY meterDbRangeChanged FINAL)
    Q_PROPERTY(int meterSize READ meterSize WRITE setMeterSize NOTIFY meterSizeChanged FINAL)

    muse::Inject<IPlaybackMeterController> meterController;
    muse::Inject<IPlaybackConfiguration> configuration;

public:
    explicit PlaybackMeterModel(QObject* parent = nullptr);

    Q_INVOKABLE double stepToPosition(double step);
    Q_INVOKABLE double sampleToPosition(double sample) const;
    Q_INVOKABLE QString sampleToText(double sample) const;

    PlaybackMeterStyle::MeterStyle meterStyle() const;
    PlaybackMeterType::MeterType meterType() const;
    PlaybackMeterPosition::MeterPosition meterPosition() const;
    PlaybackMeterDbRange::DbRange meterDbRange() const;
    int meterSize() const;

    void setMeterStyle(PlaybackMeterStyle::MeterStyle style);
    void setMeterType(PlaybackMeterType::MeterType type);
    void setMeterPosition(PlaybackMeterPosition::MeterPosition position);
    void setMeterDbRange(PlaybackMeterDbRange::DbRange range);
    void setMeterSize(int size);

    QVariantList smallSteps() const;
    QVariantList fullSteps() const;

signals:
    void smallStepsChanged();
    void fullStepsChanged();

    void meterStyleChanged();
    void meterTypeChanged();
    void meterPositionChanged();
    void meterDbRangeChanged();
    void meterSizeChanged();
};
}
