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
    for (const auto& step : meterController()->fullSteps(m_meterSize)) {
        steps.append(step);
    }

    return steps;
}

QVariantList PlaybackMeterModel::smallSteps() const
{
    QVariantList steps;
    for (const auto& step : meterController()->smallSteps(m_meterSize)) {
        steps.append(step);
    }

    return steps;
}

int PlaybackMeterModel::meterSize() const
{
    return m_meterSize;
}

void PlaybackMeterModel::setMeterSize(int size)
{
    if (m_meterSize != size) {
        m_meterSize = size;
        emit smallStepsChanged();
        emit fullStepsChanged();
    }
}
