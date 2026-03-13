/*
 * Audacity: A Digital Audio Editor
 */
#include "frequencyselectioncontroller.h"

#include "framework/global/log.h"
#include "framework/global/types/number.h"
#include "internal/numberscale.h"

namespace au::spectrogram {
FrequencySelectionController::FrequencySelectionController(const muse::modularity::ContextPtr& ctx)
    : muse::Injectable(ctx)
{
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
    IF_ASSERT_FAILED(config) {
        return;
    }

    const std::optional<int> previousTrackId
        = m_frequencySelection.isValid() ? std::make_optional(m_frequencySelection.trackId) : std::nullopt;

    const auto startFrequency = std::max(frequencySelection.startFrequency(), 0.0);
    const auto endFrequency
        = std::min(frequencySelection.endFrequency(), spectrogramService()->frequencyHardMaximum(frequencySelection.trackId));
    frequencySelection.setFrequencyRange(startFrequency, endFrequency, config->scale());

    if (m_frequencySelection != frequencySelection) {
        const auto startFrequencyChanged = !muse::is_equal(m_frequencySelection.startFrequency(), frequencySelection.startFrequency());
        const auto endFrequencyChanged = !muse::is_equal(m_frequencySelection.endFrequency(), frequencySelection.endFrequency());

        m_frequencySelection = frequencySelection;

        if (previousTrackId && previousTrackId != frequencySelection.trackId) {
            m_frequencySelectionChanged.send(*previousTrackId, complete);
            if (startFrequencyChanged) {
                m_startFrequencyChanged.send(*previousTrackId, complete);
            } else if (endFrequencyChanged) {
                m_endFrequencyChanged.send(*previousTrackId, complete);
            }
        }

        if (startFrequencyChanged) {
            m_startFrequencyChanged.send(frequencySelection.trackId, complete);
        } else if (endFrequencyChanged) {
            m_endFrequencyChanged.send(frequencySelection.trackId, complete);
        }
    }

    // Send even if not changed because the `complete` flag might be different.
    m_frequencySelectionChanged.send(frequencySelection.trackId, complete);
}

void FrequencySelectionController::setStartFrequency(int trackId, double frequency, bool complete)
{
    setEdgeFrequency(trackId, frequency, complete, true);
}

void FrequencySelectionController::setEndFrequency(int trackId, double frequency, bool complete)
{
    setEdgeFrequency(trackId, frequency, complete, false);
}

void FrequencySelectionController::setEdgeFrequency(int trackId, double frequency, const std::optional<bool>& complete, bool isStart)
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(trackId);
    IF_ASSERT_FAILED(config) {
        return;
    }

    const std::optional<int> previousTrackId
        = m_frequencySelection.isValid() ? std::make_optional(m_frequencySelection.trackId) : std::nullopt;

    const auto clampedFrequency = std::clamp(frequency, 0.0, spectrogramService()->frequencyHardMaximum(trackId));

    const auto referenceFrequency = isStart ? m_frequencySelection.startFrequency() : m_frequencySelection.endFrequency();
    if (muse::is_equal(clampedFrequency, referenceFrequency)) {
        m_frequencySelectionChanged.send(trackId, complete);
        return;
    }

    m_frequencySelection.trackId = trackId;
    if (isStart) {
        m_frequencySelection.setFrequencyRange(clampedFrequency, m_frequencySelection.endFrequency(), config->scale());
    } else {
        m_frequencySelection.setFrequencyRange(m_frequencySelection.startFrequency(), clampedFrequency, config->scale());
    }

    if (previousTrackId && *previousTrackId != trackId) {
        m_frequencySelectionChanged.send(*previousTrackId, complete);
    }

    m_frequencySelectionChanged.send(trackId, complete);
    if (isStart) {
        m_startFrequencyChanged.send(trackId, complete);
    } else {
        m_endFrequencyChanged.send(trackId, complete);
    }
}

void FrequencySelectionController::setCenterFrequency(int trackId, double newCenterFrequency)
{
    const auto config = spectrogramService()->trackSpectrogramConfiguration(trackId);
    if (!config) {
        return;
    }

    NumberScale numberScale(config->scale(), config->minFreq(), config->maxFreq());
    const auto centerFrequencyPos = numberScale.valueToPosition(newCenterFrequency);

    const auto startFreqPos = numberScale.valueToPosition(m_frequencySelection.startFrequency());
    const auto endFreqPos = numberScale.valueToPosition(m_frequencySelection.endFrequency());
    const auto range = endFreqPos - startFreqPos;
    auto newStartFreqPos = centerFrequencyPos - range / 2;
    auto newEndFreqPos = centerFrequencyPos + range / 2;

    const auto minFreqPos = numberScale.valueToPosition(config->minFreq());
    const auto maxFreqPos = numberScale.valueToPosition(config->maxFreq());
    if (newStartFreqPos > minFreqPos) {
        const auto delta = newStartFreqPos - minFreqPos;
        newEndFreqPos = std::min<double>(newEndFreqPos + delta, minFreqPos);
        newStartFreqPos = minFreqPos;
    } else if (newEndFreqPos < maxFreqPos) {
        const auto delta = maxFreqPos - newEndFreqPos;
        newStartFreqPos = std::max<double>(newStartFreqPos - delta, maxFreqPos);
        newEndFreqPos = maxFreqPos;
    }

    if (newStartFreqPos == newEndFreqPos) {
        return;
    }

    const auto newStartFreq = numberScale.positionToValue(newStartFreqPos);
    const auto newEndFreq = numberScale.positionToValue(newEndFreqPos);

    setEdgeFrequency(trackId, newStartFreq, std::nullopt, true);
    setEdgeFrequency(trackId, newEndFreq, std::nullopt, false);

    const auto previousTrackId = m_frequencySelection.trackId;
    if (previousTrackId != trackId) {
        m_centerFrequencyChanged.send(trackId);
    }
    m_centerFrequencyChanged.send(trackId);
}

void FrequencySelectionController::notifyAboutStartFrequencyChanged(int trackId, const std::optional<bool>& complete)
{
    m_startFrequencyChanged.send(trackId, complete);
    m_frequencySelectionChanged.send(trackId, complete);
}

void FrequencySelectionController::notifyAboutEndFrequencyChanged(int trackId, const std::optional<bool>& complete)
{
    m_endFrequencyChanged.send(trackId, complete);
    m_frequencySelectionChanged.send(trackId, complete);
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

muse::async::Channel<int, std::optional<bool> > FrequencySelectionController::frequencySelectionChanged() const
{
    return m_frequencySelectionChanged;
}

muse::async::Channel<int, std::optional<bool> > FrequencySelectionController::startFrequencyChanged() const
{
    return m_startFrequencyChanged;
}

muse::async::Channel<int, std::optional<bool> > FrequencySelectionController::endFrequencyChanged() const
{
    return m_endFrequencyChanged;
}

muse::async::Channel<int> FrequencySelectionController::centerFrequencyChanged() const
{
    return m_centerFrequencyChanged;
}
}
