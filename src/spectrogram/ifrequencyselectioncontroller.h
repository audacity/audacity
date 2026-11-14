/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/channel.h"

namespace au::spectrogram {
/**
 * @brief Models Audacity's frequency selection.
 *
 * @details Spectral selection spans one track at a time. (May be extended in the future.)
 * Call `beginSelection` to get started.
 * "Drag" frequency of returned handle - this will create a non-empty frequency selection with the
 * begin frequency as anchor.
 * Modify the selection by grabbing either start or end frequency handle and then dragging it.
 * Handles can be crossed, swapping roles (start frequency becomes end frequency and vice-versa).
 * The selection can also be modified by its center, in which case no handle is needed since it can't cause edges to cross.
 */
class IFrequencySelectionController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IFrequencySelectionController)

public:
    virtual FrequencySelection frequencySelection() const = 0;
    virtual ~IFrequencySelectionController() = default;

    virtual bool showsSpectrogram(int trackId) const = 0;
    virtual void setShowsSpectrogram(int trackId, bool value) = 0;

    /**
     * @brief starts a frequency selection, returning a frequency handle to move around.
     */
    virtual uintptr_t beginSelection(int trackId, double frequency) = 0;
    virtual void resetFrequencySelection() = 0;

    /**
     * @brief Restores frequency selection after undo/redo.
     *
     * @details Each frequency-selection change is stored on the persistence object.
     * When undo/redo is called, the persistence object is reset to another history state.
     * This method loads this state in the controller.
     */
    virtual void restoreFrequencySelection() = 0;

    virtual uintptr_t startFrequencyHandle() const = 0;
    virtual uintptr_t endFrequencyHandle() const = 0;
    /**
     * @brief Sets the frequency of the given handle or of both if handle is 0.
     */
    virtual void setHandleFrequency(double frequency, bool complete, uintptr_t handle = 0) = 0;
    virtual double handleFrequency(uintptr_t handle) const = 0;

    /**
     * @brief Modifies the selection's center frequency. Range is preserved in the units of the track's spectrogram scale,
     * i.e., Hz for a linear scale, but barks on a Bark scale, mels on a Mel scale, etc.
     */
    virtual void setCenterFrequency(double frequency, bool complete) = 0;

    virtual muse::async::Channel<bool /* complete */> frequencySelectionChanged() const = 0;
    virtual muse::async::Channel<uintptr_t, bool /* complete */> handleDragged() const = 0;
};
}
