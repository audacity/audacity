/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackmetercontroller.h"
#include "playback/iplayback.h"
#include "record/irecord.h"

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
    Q_PROPERTY(int meterSize READ meterSize WRITE setMeterSize NOTIFY meterSizeChanged FINAL)

    Q_PROPERTY(
        PlaybackMeterDbRange::DbRange meterDbRange READ meterDbRange WRITE setMeterDbRange NOTIFY meterDbRangeChanged FINAL)

    Q_PROPERTY(std::vector<PlaybackMeterDbRange::DbRange> dbRangeList READ dbRangeList CONSTANT)
    Q_PROPERTY(float dbRange READ dbRange NOTIFY dbRangeChanged FINAL)

    Q_PROPERTY(
        MeterInputSource::Source meterInputSource READ meterInputSource WRITE setMeterInputSource NOTIFY meterInputSourceChanged FINAL)

    Q_PROPERTY(float position READ position NOTIFY positionChanged FINAL)

    muse::Inject<IPlaybackMeterController> meterController;
    muse::Inject<IPlaybackConfiguration> configuration;
    muse::Inject<IPlayback> playback;
    muse::Inject<record::IRecord> record;

public:
    explicit PlaybackMeterModel(QObject* parent = nullptr);

    Q_INVOKABLE double stepToPosition(double step);
    Q_INVOKABLE double sampleToPosition(double sample) const;
    Q_INVOKABLE double positionToSample(double position) const;
    Q_INVOKABLE QString sampleToText(double sample) const;
    Q_INVOKABLE QString description(PlaybackMeterDbRange::DbRange range) const;

    PlaybackMeterStyle::MeterStyle meterStyle() const;
    PlaybackMeterType::MeterType meterType() const;
    PlaybackMeterPosition::MeterPosition meterPosition() const;
    int meterSize() const;

    PlaybackMeterDbRange::DbRange meterDbRange() const;
    std::vector<PlaybackMeterDbRange::DbRange> dbRangeList() const;
    float dbRange() const;

    MeterInputSource::Source meterInputSource() const;

    float position() const;

    void setMeterStyle(PlaybackMeterStyle::MeterStyle style);
    void setMeterType(PlaybackMeterType::MeterType type);
    void setMeterPosition(PlaybackMeterPosition::MeterPosition position);
    void setMeterSize(int size);

    void setMeterDbRange(PlaybackMeterDbRange::DbRange range);

    void setMeterInputSource(MeterInputSource::Source source);

    QVariantList smallSteps() const;
    QVariantList fullSteps() const;

signals:
    void smallStepsChanged();
    void fullStepsChanged();

    void meterStyleChanged();
    void meterTypeChanged();
    void meterPositionChanged();
    void meterSizeChanged();

    void meterDbRangeChanged();
    void dbRangeChanged();

    void meterInputSourceChanged();

    void positionChanged();

private:
    float m_volume = 0.0f;
    int m_meterSize = 0;
    MeterInputSource::Source m_meterInputSource = MeterInputSource::Source::Playback;
};
}
