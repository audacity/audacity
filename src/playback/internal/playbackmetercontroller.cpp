/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmetercontroller.h"

#include <memory>

#include "playback/internal/meters/dblogmeter.h"
#include "playback/internal/meters/dblinearmeter.h"
#include "playback/internal/meters/linearmeter.h"

using namespace au::playback;

namespace {
std::shared_ptr<IPlaybackMeter> createMeter(PlaybackMeterType::MeterType meterType, int meterSize, double dbRange)
{
    switch (meterType) {
    case PlaybackMeterType::MeterType::DbLinear:
        return std::make_shared<DbLinearMeter>(meterSize, dbRange);
    case PlaybackMeterType::MeterType::DbLog:
        return std::make_shared<DbLogMeter>(meterSize, dbRange);
    case PlaybackMeterType::MeterType::Linear:
        return std::make_shared<LinearMeter>(meterSize, dbRange);
    default:
        return std::make_shared<DbLogMeter>(meterSize, dbRange);
    }
}
}

PlaybackMeterController::PlaybackMeterController()
{
    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        m_meter = createMeter(configuration()->playbackMeterType(),
                              configuration()->playbackHorizontalMeterSize(),
                              PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));
        m_playbackMeterChanged.notify();
    });

    configuration()->playbackMeterDbRangeChanged().onNotify(this, [this]() {
        m_meter->setDbRange(PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));
        m_playbackMeterChanged.notify();
    });

    configuration()->playbackHorizontalMeterSizeChanged().onNotify(this, [this]() {
        m_meter->setMeterSize(configuration()->playbackHorizontalMeterSize());
        m_playbackMeterChanged.notify();
    });

    m_meter = createMeter(configuration()->playbackMeterType(),
                          configuration()->playbackHorizontalMeterSize(),
                          PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));
}

double PlaybackMeterController::stepToPosition(double step) const
{
    return m_meter->stepToPosition(step);
}

double PlaybackMeterController::sampleToPosition(double sample) const
{
    return m_meter->sampleToPosition(sample);
}

double PlaybackMeterController::positionToSample(double position) const
{
    return m_meter->positionToSample(position);
}

std::string PlaybackMeterController::sampleToText(double sample) const
{
    return m_meter->sampleToText(sample);
}

std::vector<double> PlaybackMeterController::fullSteps() const
{
    return m_meter->fullSteps();
}

std::vector<double> PlaybackMeterController::smallSteps() const
{
    return m_meter->smallSteps();
}

muse::async::Notification PlaybackMeterController::playbackMeterChanged() const
{
    return m_playbackMeterChanged;
}
