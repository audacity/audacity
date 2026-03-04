/*
 * Audacity: A Digital Audio Editor
 */
#include "frequencyselectioncontroller.h"

#include "framework/global/defer.h"

// clip selection

namespace au::spectrogram {
FrequencySelectionController::FrequencySelectionController(const muse::modularity::ContextPtr& ctx)
    : muse::Injectable(ctx)
{
}

FrequencySelection FrequencySelectionController::frequencySelection() const
{
    return m_frequencySelection;
}

void FrequencySelectionController::setFrequencySelection(FrequencySelection frequencySelection)
{
    const std::optional<int> previousTrackId
        = m_frequencySelection.isValid() ? std::make_optional(m_frequencySelection.trackId) : std::nullopt;

    if (m_frequencySelection == frequencySelection) {
        return;
    }

    muse::Defer notifyPreviousTrack([this, trackId = frequencySelection.trackId, previousTrackId]() {
        if (previousTrackId && previousTrackId != trackId) {
            m_frequencySelectionChanged.send(*previousTrackId);
        }
    });

    m_frequencySelection = frequencySelection;
    m_frequencySelectionChanged.send(frequencySelection.trackId);
}

void FrequencySelectionController::resetFrequencySelection()
{
    if (!m_frequencySelection.isValid()) {
        return;
    }

    const int previousTrackId = m_frequencySelection.trackId;

    m_frequencySelection = FrequencySelection{};
    m_frequencySelectionChanged.send(previousTrackId);
}

muse::async::Channel<int> FrequencySelectionController::frequencySelectionChanged() const
{
    return m_frequencySelectionChanged;
}
}
