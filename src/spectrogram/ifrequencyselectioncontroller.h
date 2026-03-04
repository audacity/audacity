/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/channel.h"

namespace au::spectrogram {
class IFrequencySelectionController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IFrequencySelectionController)

public:
    virtual FrequencySelection frequencySelection() const = 0;

    virtual bool showsSpectrogram(int trackId) const = 0;
    virtual void setShowsSpectrogram(int trackId, bool value) = 0;

    virtual void setFrequencySelection(FrequencySelection) = 0;
    virtual void resetFrequencySelection() = 0;
    virtual muse::async::Channel<int /*track ID*/> frequencySelectionChanged() const = 0;
};
}
