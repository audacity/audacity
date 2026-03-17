/*
 * Audacity: A Digital Audio Editor
 */
#include "frequencyselectioncontroller.h"

#include "ifrequencyselectionrestorer.h"

#include "framework/global/defer.h"
#include "framework/global/log.h"

namespace au::spectrogram {
FrequencySelectionController::FrequencySelectionController(const muse::modularity::ContextPtr& ctx,
                                                           std::unique_ptr<IFrequencySelectionRestorer> frequencySelectionRestorer)
    : muse::Injectable(ctx), m_frequencySelectionRestorer(std::move(frequencySelectionRestorer))
{
    m_frequencySelectionChanged.onReceive(this, [this](int, bool complete) {
        if (complete) {
            m_frequencySelectionRestorer->storeFrequencySelectionState(m_frequencySelection);
        }
    });
}

void FrequencySelectionController::restoreFrequencySelection()
{
    const auto restoredSelection = m_frequencySelectionRestorer->loadFrequencySelectionState();
    if (restoredSelection.isValid()) {
        setFrequencySelection(restoredSelection, true);
    }
}

bool FrequencySelectionController::showsSpectrogram(int trackId) const
{
    return m_tracksWithSpectrogramShown.find(trackId) != m_tracksWithSpectrogramShown.end();
}

void FrequencySelectionController::setShowsSpectrogram(int trackId, bool value)
{
    if (value) {
        m_tracksWithSpectrogramShown.insert(trackId);
    } else {
        m_tracksWithSpectrogramShown.erase(trackId);
    }
}

FrequencySelection FrequencySelectionController::frequencySelection() const
{
    return m_frequencySelection;
}

void FrequencySelectionController::setFrequencySelection(FrequencySelection frequencySelection, bool complete)
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(frequencySelection.trackId);
    if (!config) {
        return;
    }

    const std::optional<int> previousTrackId
        = m_frequencySelection.isValid() ? std::make_optional(m_frequencySelection.trackId) : std::nullopt;

    const auto startFrequency = std::max(frequencySelection.startFrequency(), 0.0);
    const auto endFrequency
        = std::min(frequencySelection.endFrequency(), spectrogramService()->frequencyHardMaximum(frequencySelection.trackId));
    frequencySelection.setFrequencyRange(startFrequency, endFrequency, config->scale());

    if (m_frequencySelection != frequencySelection) {
        muse::Defer notifyPreviousTrack([this, trackId = frequencySelection.trackId, previousTrackId, complete]() {
            if (previousTrackId && previousTrackId != trackId) {
                m_frequencySelectionChanged.send(*previousTrackId, complete);
            }
        });

        m_frequencySelection = frequencySelection;
    }

    m_frequencySelectionChanged.send(frequencySelection.trackId, complete);
}

void FrequencySelectionController::resetFrequencySelection()
{
    if (m_frequencySelection.trackId == -1) {
        return;
    }

    const int previousTrackId = m_frequencySelection.trackId;

    m_frequencySelection = FrequencySelection{};
    m_frequencySelectionChanged.send(previousTrackId, true);
}

muse::async::Channel<int, bool> FrequencySelectionController::frequencySelectionChanged() const
{
    return m_frequencySelectionChanged;
}
}
