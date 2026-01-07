/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itrackspectrogramconfiguration.h"

#include "framework/global/modularity/imoduleinterface.h"

namespace au::spectrogram {
class ISpectrogramService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramService)

public:
    virtual ~ISpectrogramService() = default;

    virtual ITrackSpectrogramConfigurationPtr trackSpectrogramConfiguration(int trackId) const = 0;
    virtual void copyConfiguration(const ISpectrogramConfiguration& source, ISpectrogramConfiguration& destination) const = 0;
    virtual double yToFrequency(int trackId, double spectrogramY, double spectrogramHeight) const = 0;
};
}
