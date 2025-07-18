/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmetermodel.h"

#include <memory>

using namespace au::playback;

PlaybackMeterModel::PlaybackMeterModel(QObject* parent)
    : QObject(parent)
{
    meterController()->playbackMeterChanged().onNotify(this, [this]() {
        emit smallStepsChanged();
        emit fullStepsChanged();
    });

    configuration()->playbackMeterPositionChanged().onNotify(this, [this]() {
        emit meterPositionChanged();
    });

    configuration()->playbackMeterStyleChanged().onNotify(this, [this]() {
        emit meterStyleChanged();
    });

    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        emit meterTypeChanged();
    });

    configuration()->playbackHorizontalMeterSizeChanged().onNotify(this, [this]() {
        emit meterSizeChanged();
    });

    configuration()->playbackMeterDbRangeChanged().onNotify(this, [this]() {
        emit meterDbRangeChanged();
    });

    m_dbRanges = new PlaybackMeterDbRangeModel(this);
    emit dbRangesChanged();
}

double PlaybackMeterModel::stepToPosition(double step)
{
    return meterController()->stepToPosition(step);
}

double PlaybackMeterModel::sampleToPosition(double sample) const
{
    return meterController()->sampleToPosition(sample);
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

muse::uicomponents::MenuItemList PlaybackMeterModel::dbRanges() const
{
    m_dbRanges->load();
    return m_dbRanges->items();
}

QString PlaybackMeterModel::currentDbRange() const
{
    switch (meterDbRange()) {
    case PlaybackMeterDbRange::DbRange::Range36:
        return QString::fromUtf8("-36 dB (shallow range for high-amplitude editing)");
    case PlaybackMeterDbRange::DbRange::Range48:
        return QString::fromUtf8("-48 dB (PCM range of 8 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range60:
        return QString::fromUtf8("-60 dB (PCM range of 10 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range72:
        return QString::fromUtf8("-72 dB (PCM range of 12 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range84:
        return QString::fromUtf8("-84 dB (PCM range of 14 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range96:
        return QString::fromUtf8("-96 dB (PCM range of 16 bit samples)");
    case PlaybackMeterDbRange::DbRange::Range120:
        return QString::fromUtf8("-120 dB (approximate limit of human hearing)");
    case PlaybackMeterDbRange::DbRange::Range145:
        return QString::fromUtf8("-145 dB (PCM range of 24 bit samples)");
    default:
        return QString::fromUtf8("");
    }
}

void PlaybackMeterModel::handleDbRangeChange(const QString& itemId)
{
    if (itemId.isEmpty()) {
        return;
    }

    if (itemId == "meter-db-range-36") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range36);
        emit dbRangesChanged();
        return;
    }

    if (itemId == "meter-db-range-48") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range48);
        emit dbRangesChanged();
        return;
    }

    if (itemId == "meter-db-range-60") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range60);
        emit dbRangesChanged();
        return;
    }

    if (itemId == "meter-db-range-72") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range72);
        emit dbRangesChanged();
        return;
    }

    if (itemId == "meter-db-range-84") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range84);
        emit dbRangesChanged();
        return;
    }

    if (itemId == "meter-db-range-96") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range96);
        emit dbRangesChanged();
        return;
    }

    if (itemId == "meter-db-range-120") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range120);
        emit dbRangesChanged();
        return;
    }

    if (itemId == "meter-db-range-145") {
        setMeterDbRange(PlaybackMeterDbRange::DbRange::Range145);
        emit dbRangesChanged();
        return;
    }
}
