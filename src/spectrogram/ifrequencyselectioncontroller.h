/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/channel.h"

#include <optional>

namespace au::spectrogram {
class IFrequencySelectionController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IFrequencySelectionController)

public:
    virtual FrequencySelection frequencySelection() const = 0;
    virtual ~IFrequencySelectionController() = default;

    virtual bool showsSpectrogram(int trackId) const = 0;
    virtual void setShowsSpectrogram(int trackId, bool value) = 0;

    virtual void setStartFrequency(int trackId, double frequency, bool complete) = 0;
    virtual void setEndFrequency(int trackId, double frequency, bool complete) = 0;
    virtual void setCenterFrequency(int trackId, double frequency) = 0;

    virtual void setFrequencySelection(FrequencySelection, bool complete) = 0;
    virtual void resetFrequencySelection() = 0;
    virtual muse::async::Channel<int /*track ID*/, std::optional<bool> /* complete */> frequencySelectionChanged() const = 0;
    virtual muse::async::Channel<int /*track ID*/, std::optional<bool> /* complete */> startFrequencyChanged() const = 0;
    virtual muse::async::Channel<int /*track ID*/, std::optional<bool> /* complete */> endFrequencyChanged() const = 0;
    virtual muse::async::Channel<int /*track ID*/> centerFrequencyChanged() const = 0;
};
}
