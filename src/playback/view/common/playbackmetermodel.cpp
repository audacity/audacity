/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmetermodel.h"

#include "playback/iaudiooutput.h"

#include <memory>

using namespace au::playback;

namespace {
constexpr const char* DB36_DESCRIPTION = "-36 dB (shallow range for high-amplitude editing)";
constexpr const char* DB48_DESCRIPTION = "-48 dB (PCM range of 8 bit samples)";
constexpr const char* DB60_DESCRIPTION = "-60 dB (PCM range of 10 bit samples)";
constexpr const char* DB72_DESCRIPTION = "-72 dB (PCM range of 12 bit samples)";
constexpr const char* DB84_DESCRIPTION = "-84 dB (PCM range of 14 bit samples)";
constexpr const char* DB96_DESCRIPTION = "-96 dB (PCM range of 16 bit samples)";
constexpr const char* DB120_DESCRIPTION = "-120 dB (approximate limit of human hearing)";
constexpr const char* DB144_DESCRIPTION = "-145 dB (PCM range of 24 bit samples)";

constexpr float LINEAR_METER_MIN_VOLUME = -60.0f;
}

PlaybackMeterModel::PlaybackMeterModel(QObject* parent)
    : QObject(parent)
{
    meterController()->playbackMeterChanged().onNotify(this, [this]() {
        emit smallStepsChanged();
        emit fullStepsChanged();
        emit positionChanged();
    });

    configuration()->playbackMeterPositionChanged().onNotify(this, [this]() {
        emit meterPositionChanged();
    });

    configuration()->playbackMeterStyleChanged().onNotify(this, [this]() {
        emit meterStyleChanged();
    });

    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        emit meterTypeChanged();
        emit positionChanged();
    });

    configuration()->playbackHorizontalMeterSizeChanged().onNotify(this, [this]() {
        emit meterSizeChanged();
    });

    configuration()->playbackMeterDbRangeChanged().onNotify(this, [this]() {
        emit meterDbRangeChanged();
        emit dbRangeChanged();
        emit positionChanged();
    });

    playback()->audioOutput()->playbackVolumeChanged().onReceive(this, [this](audio::volume_dbfs_t volume) {
        m_volume = volume;
        emit positionChanged();
    });

    playback()->audioOutput()->playbackVolume().onResolve(this, [this](float volume) {
        m_volume = volume;
        emit positionChanged();
    });
}

double PlaybackMeterModel::stepToPosition(double step)
{
    return meterController()->stepToPosition(step);
}

double PlaybackMeterModel::sampleToPosition(double sample) const
{
    return meterController()->sampleToPosition(sample);
}

double PlaybackMeterModel::positionToSample(double position) const
{
    return meterController()->positionToSample(position);
}

QString PlaybackMeterModel::sampleToText(double sample) const
{
    return QString::fromStdString(meterController()->sampleToText(sample));
}

QVariantList PlaybackMeterModel::fullSteps() const
{
    QVariantList steps;
    for (const auto& step : meterController()->fullSteps()) {
        steps.append(step);
    }

    return steps;
}

QVariantList PlaybackMeterModel::smallSteps() const
{
    QVariantList steps;
    for (const auto& step : meterController()->smallSteps()) {
        steps.append(step);
    }

    return steps;
}

void PlaybackMeterModel::setMeterStyle(PlaybackMeterStyle::MeterStyle style)
{
    if (meterStyle() == style) {
        return;
    }

    configuration()->setPlaybackMeterStyle(style);
}

PlaybackMeterStyle::MeterStyle PlaybackMeterModel::meterStyle() const
{
    return configuration()->playbackMeterStyle();
}

void PlaybackMeterModel::setMeterType(PlaybackMeterType::MeterType type)
{
    if (meterType() == type) {
        return;
    }

    configuration()->setPlaybackMeterType(type);
}

PlaybackMeterType::MeterType PlaybackMeterModel::meterType() const
{
    return configuration()->playbackMeterType();
}

void PlaybackMeterModel::setMeterPosition(PlaybackMeterPosition::MeterPosition position)
{
    if (meterPosition() == position) {
        return;
    }

    configuration()->setPlaybackMeterPosition(position);
}

PlaybackMeterPosition::MeterPosition PlaybackMeterModel::meterPosition() const
{
    return configuration()->playbackMeterPosition();
}

void PlaybackMeterModel::setMeterSize(int size)
{
    if (meterSize() == size) {
        return;
    }

    configuration()->setPlaybackHorizontalMeterSize(size);
    emit smallStepsChanged();
    emit fullStepsChanged();
}

int PlaybackMeterModel::meterSize() const
{
    return configuration()->playbackHorizontalMeterSize();
}

void PlaybackMeterModel::setMeterDbRange(PlaybackMeterDbRange::DbRange range)
{
    if (meterDbRange() == range) {
        return;
    }

    configuration()->setPlaybackMeterDbRange(range);
}

PlaybackMeterDbRange::DbRange PlaybackMeterModel::meterDbRange() const
{
    return configuration()->playbackMeterDbRange();
}

float PlaybackMeterModel::dbRange() const
{
    if (meterType() == PlaybackMeterType::MeterType::Linear) {
        return LINEAR_METER_MIN_VOLUME;
    }

    return PlaybackMeterDbRange::toDouble(meterDbRange());
}

std::vector<PlaybackMeterDbRange::DbRange> PlaybackMeterModel::dbRangeList() const
{
    std::vector<PlaybackMeterDbRange::DbRange> ranges;

    ranges.push_back(PlaybackMeterDbRange::DbRange::Range36);
    ranges.push_back(PlaybackMeterDbRange::DbRange::Range48);
    ranges.push_back(PlaybackMeterDbRange::DbRange::Range60);
    ranges.push_back(PlaybackMeterDbRange::DbRange::Range72);
    ranges.push_back(PlaybackMeterDbRange::DbRange::Range84);
    ranges.push_back(PlaybackMeterDbRange::DbRange::Range96);
    ranges.push_back(PlaybackMeterDbRange::DbRange::Range120);
    ranges.push_back(PlaybackMeterDbRange::DbRange::Range144);

    return ranges;
}

float PlaybackMeterModel::position() const
{
    return sampleToPosition(m_volume);
}

QString PlaybackMeterModel::description(PlaybackMeterDbRange::DbRange range) const
{
    switch (range) {
    case PlaybackMeterDbRange::DbRange::Range36:
        return DB36_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range48:
        return DB48_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range60:
        return DB60_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range72:
        return DB72_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range84:
        return DB84_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range96:
        return DB96_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range120:
        return DB120_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range144:
        return DB144_DESCRIPTION;
    }

    return "";
}
