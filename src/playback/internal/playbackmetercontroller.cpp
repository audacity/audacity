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
std::shared_ptr<IPlaybackMeter> createMeter(PlaybackMeterType::MeterType meterType)
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
}

PlaybackMeterController::PlaybackMeterController()
{
    configuration()->playbackMeterTypeChanged().onNotify(this, [this]() {
        m_meter = createMeter(configuration()->playbackMeterType());
        m_playbackMeterChanged.notify();
    });

    m_meter = createMeter(configuration()->playbackMeterType());
}

double PlaybackMeterController::stepToPosition(double step) const
{
    return m_meter->stepToPosition(step);
}

double PlaybackMeterController::sampleToPosition(double sample) const
{
    return m_meter->sampleToPosition(sample);
}

std::string PlaybackMeterController::sampleToText(double sample) const
{
    return m_meter->sampleToText(sample);
}

std::vector<double> PlaybackMeterController::fullSteps(int meterSize) const
{
    return m_meter->fullSteps(meterSize);
}

std::vector<double> PlaybackMeterController::smallSteps(int meterSize) const
{
    return m_meter->smallSteps(meterSize);
}

muse::async::Notification PlaybackMeterController::playbackMeterChanged() const
{
    return m_playbackMeterChanged;
}
