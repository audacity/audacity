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
        qDebug() << "PlaybackMeterModel: playbackMeterPositionChanged to "
                 << configuration()->playbackMeterPosition();
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
    for (const auto& step : meterController()->fullSteps(meterSize())) {
        steps.append(step);
    }

    return steps;
}

QVariantList PlaybackMeterModel::smallSteps() const
{
    QVariantList steps;
    for (const auto& step : meterController()->smallSteps(meterSize())) {
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
