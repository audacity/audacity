/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmetercontroller.h"

#include <memory>

#include "playback/internal/meterstrategy/dblogmeter.h"
#include "playback/internal/meterstrategy/dblinearmeter.h"
#include "playback/internal/meterstrategy/linearmeter.h"

using namespace au::playback;

static std::shared_ptr<IPlaybackMeterStrategy> createMeterStrategy(PlaybackMeterType::MeterType meterType)
{
    switch (meterType) {
    case PlaybackMeterType::MeterType::DbLinear:
        return std::make_shared<DbLinearMeter>();
    case PlaybackMeterType::MeterType::DbLog:
        return std::make_shared<DbLogMeter>();
    case PlaybackMeterType::MeterType::Linear:
        return std::make_shared<LinearMeter>();
    default:
        return std::make_shared<DbLogMeter>();
    }
}

PlaybackMeterController::PlaybackMeterController()
{
    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        m_strategy = createMeterStrategy(configuration()->playbackMeterType());
        m_playbackMeterChanged.notify();
    });

    createMeterStrategy(configuration()->playbackMeterType());
}

double PlaybackMeterController::stepToPosition(double step) const
{
    return m_strategy->stepToPosition(step);
}

double PlaybackMeterController::sampleToPosition(double sample) const
{
    return m_strategy->sampleToPosition(sample);
}

std::string PlaybackMeterController::sampleToText(double sample) const
{
    return m_strategy->sampleToText(sample);
}

std::vector<double> PlaybackMeterController::fullSteps(int meterSize) const
{
    return m_strategy->fullSteps(meterSize);
}

std::vector<double> PlaybackMeterController::smallSteps(int meterSize) const
{
    return m_strategy->smallSteps(meterSize);
}

muse::async::Notification PlaybackMeterController::playbackMeterChanged() const
{
    return m_playbackMeterChanged;
}
