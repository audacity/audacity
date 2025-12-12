/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramconfiguration.h"

#include <memory>

namespace au::spectrogram {
class ITrackSpectrogramConfiguration : public ISpectrogramConfiguration
{
public:
    virtual ~ITrackSpectrogramConfiguration() = default;

    virtual bool useGlobalSettings() const = 0;
    virtual void setUseGlobalSettings(bool value) = 0;
};

using ITrackSpectrogramConfigurationPtr = std::shared_ptr<ITrackSpectrogramConfiguration>;
} // namespace au::spectrogram
