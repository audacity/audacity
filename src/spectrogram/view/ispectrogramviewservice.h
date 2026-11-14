/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/async/channel.h"
#include "framework/global/modularity/imoduleinterface.h"

namespace au::spectrogram {
class ISpectrogramViewService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramViewService)

public:
    virtual ~ISpectrogramViewService() = default;

    virtual double rulerGuideFrequency(int trackId) const = 0;
    virtual void setRulerGuideFrequency(int trackId, double frequency) = 0;
    virtual muse::async::Channel<int /* track id */> rulerGuideFrequencyChanged() const = 0;
};
}
