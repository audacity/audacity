/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itrackspectrogramconfiguration.h"

#include "framework/global/modularity/imoduleinterface.h"

namespace au::spectrogram {
class ITrackSpectrogramConfigurationProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackSpectrogramConfigurationProvider)

public:
    virtual ~ITrackSpectrogramConfigurationProvider() = default;

    virtual ITrackSpectrogramConfigurationPtr trackSpectrogramConfiguration(int trackId) const = 0;
    virtual void copyConfiguration(const ISpectrogramConfiguration& source, ISpectrogramConfiguration& destination) const = 0;
};
}
