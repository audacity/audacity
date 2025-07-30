/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "playback/iplaybackmetercontroller.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iplaybackmeter.h"

namespace au::playback {
class PlaybackMeterController : public IPlaybackMeterController, public muse::async::Asyncable
{
    muse::Inject<IPlaybackConfiguration> configuration;

public:
    PlaybackMeterController();

    double stepToPosition(double sample) const override;
    double sampleToPosition(double sample) const override;
    double positionToSample(double position) const override;
    std::string sampleToText(double sample) const override;

    std::vector<double> smallSteps() const override;
    std::vector<double> fullSteps() const override;

    muse::async::Notification playbackMeterChanged() const override;

private:
    std::shared_ptr<IPlaybackMeter> m_meter;
    muse::async::Notification m_playbackMeterChanged;
};
}
