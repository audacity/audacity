/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itrackspectrogramconfiguration.h"

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"

namespace au::spectrogram {
class ISpectrogramService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramService)

public:
    virtual ~ISpectrogramService() = default;

    virtual ITrackSpectrogramConfigurationPtr trackSpectrogramConfiguration(int trackId) const = 0;
    virtual muse::async::Channel<int /*track id*/> trackSpectrogramConfigurationChanged() const = 0;
    virtual void notifyAboutTrackSpectrogramConfigurationChanged(int trackId) = 0;

    virtual void copyConfiguration(ISpectrogramConfiguration& source, ISpectrogramConfiguration& destination) const = 0;
    virtual double frequencyHardMaximum(int trackId) const = 0;
    virtual double yToFrequency(int trackId, double spectrogramY, double spectrogramHeight) const = 0;
    virtual double frequencyToY(int trackId, double frequency, double spectrogramHeight) const = 0;
};
}
