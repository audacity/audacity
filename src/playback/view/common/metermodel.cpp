/*
* Audacity: A Digital Audio Editor
*/

#include "metermodel.h"

#include <memory>

#include "translation.h"

using namespace au::playback;

namespace {
constexpr float LINEAR_METER_MIN_VOLUME = -60.0f;

//! The same context these strings are already translated under. They come
//! from Audacity 3's wave track settings, and the translations shipped in
//! share/locale are filed against "wave-track-settings"; looking them up
//! under any other context would silently find nothing.
constexpr const char* DB_RANGE_CONTEXT = "wave-track-settings";
}

MeterModel::MeterModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void MeterModel::init()
{
    meterController()->playbackMeterChanged().onNotify(this, [this]() {
        emit smallStepsChanged();
        emit fullStepsChanged();
        emit positionChanged();
    });

    playbackUiState()->playbackMeterPositionChanged().onNotify(this, [this]() {
        emit meterPositionChanged();
    });

    configuration()->playbackMeterStyleChanged().onNotify(this, [this]() {
        emit meterStyleChanged();
    });

    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        emit meterTypeChanged();
        emit positionChanged();
    });

    configuration()->playbackMeterDbRangeChanged().onNotify(this, [this]() {
        emit meterDbRangeChanged();
        emit dbRangeChanged();
        emit positionChanged();
    });
}

double MeterModel::stepToPosition(double step)
{
    return meterController()->stepToPosition(step);
}

double MeterModel::sampleToPosition(double sample) const
{
    return meterController()->sampleToPosition(sample);
}

double MeterModel::positionToSample(double position) const
{
    return meterController()->positionToSample(position);
}

QString MeterModel::sampleToText(double sample) const
{
    return QString::fromStdString(meterController()->sampleToText(sample));
}

QVariantList MeterModel::fullSteps() const
{
    QVariantList steps;
    for (const auto& step : meterController()->fullSteps(m_meterSize)) {
        steps.append(step);
    }

    return steps;
}

QVariantList MeterModel::smallSteps() const
{
    QVariantList steps;
    for (const auto& step : meterController()->smallSteps(m_meterSize)) {
        steps.append(step);
    }

    return steps;
}

void MeterModel::setMeterStyle(PlaybackMeterStyle::MeterStyle style)
{
    if (meterStyle() == style) {
        return;
    }

    configuration()->setPlaybackMeterStyle(style);
}

PlaybackMeterStyle::MeterStyle MeterModel::meterStyle() const
{
    return configuration()->playbackMeterStyle();
}

void MeterModel::setMeterType(PlaybackMeterType::MeterType type)
{
    if (meterType() == type) {
        return;
    }

    configuration()->setPlaybackMeterType(type);
}

PlaybackMeterType::MeterType MeterModel::meterType() const
{
    return configuration()->playbackMeterType();
}

void MeterModel::setMeterPosition(PlaybackMeterPosition::MeterPosition position)
{
    if (meterPosition() == position) {
        return;
    }

    playbackUiState()->setPlaybackMeterPosition(position);
}

PlaybackMeterPosition::MeterPosition MeterModel::meterPosition() const
{
    return playbackUiState()->playbackMeterPosition();
}

void MeterModel::setMeterSize(int size)
{
    if (m_meterSize == size) {
        return;
    }

    m_meterSize = size;
    emit smallStepsChanged();
    emit fullStepsChanged();
}

int MeterModel::meterSize() const
{
    return m_meterSize;
}

void MeterModel::setMeterDbRange(PlaybackMeterDbRange::DbRange range)
{
    if (meterDbRange() == range) {
        return;
    }

    configuration()->setPlaybackMeterDbRange(range);
}

PlaybackMeterDbRange::DbRange MeterModel::meterDbRange() const
{
    return configuration()->playbackMeterDbRange();
}

float MeterModel::dbRange() const
{
    if (meterType() == PlaybackMeterType::MeterType::Linear) {
        return LINEAR_METER_MIN_VOLUME;
    }

    return PlaybackMeterDbRange::toDouble(meterDbRange());
}

std::vector<PlaybackMeterDbRange::DbRange> MeterModel::dbRangeList() const
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

float MeterModel::position() const
{
    return sampleToPosition(m_volume);
}

void MeterModel::setVolume(float volume)
{
    if (m_volume == volume) {
        return;
    }

    m_volume = volume;
    emit positionChanged();
}

float MeterModel::volume() const
{
    return m_volume;
}

QString MeterModel::description(PlaybackMeterDbRange::DbRange range) const
{
    switch (range) {
    case PlaybackMeterDbRange::DbRange::Range36:
        return muse::qtrc(DB_RANGE_CONTEXT, "-36 dB (shallow range for high-amplitude editing)");
    case PlaybackMeterDbRange::DbRange::Range48:
        return muse::qtrc(DB_RANGE_CONTEXT, "-48 dB (PCM range of 8 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range60:
        return muse::qtrc(DB_RANGE_CONTEXT, "-60 dB (PCM range of 10 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range72:
        return muse::qtrc(DB_RANGE_CONTEXT, "-72 dB (PCM range of 12 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range84:
        return muse::qtrc(DB_RANGE_CONTEXT, "-84 dB (PCM range of 14 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range96:
        return muse::qtrc(DB_RANGE_CONTEXT, "-96 dB (PCM range of 16 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range120:
        return muse::qtrc(DB_RANGE_CONTEXT, "-120 dB (approximate limit of human hearing)");
    case PlaybackMeterDbRange::DbRange::Range144:
        return muse::qtrc(DB_RANGE_CONTEXT, "-145 dB (PCM range of 24 bit samples)");
    }

    return "";
}
