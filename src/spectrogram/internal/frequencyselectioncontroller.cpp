/*
 * Audacity: A Digital Audio Editor
 */
#include "frequencyselectioncontroller.h"

#include "ifrequencyselectionrestorer.h"
#include "shared/axis/numberscale.h"

#include "framework/global/log.h"
#include "framework/global/defer.h"
#include "framework/global/types/number.h"

namespace au::spectrogram {
FrequencySelectionController::FrequencySelectionController(const muse::modularity::ContextPtr& ctx,
                                                           std::unique_ptr<IFrequencySelectionRestorer> frequencySelectionRestorer)
    : muse::Contextable(ctx), m_frequencySelectionRestorer(std::move(frequencySelectionRestorer))
{
}

void FrequencySelectionController::init()
{
    m_frequencySelectionChanged.onReceive(this, [this](bool complete) {
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

uintptr_t FrequencySelectionController::beginSelection(int trackId, double frequency)
{
    m_config = spectrogramService()->trackSpectrogramConfiguration(trackId);
    if (!m_config) {
        return 0;
    }

    m_frequencySelection = FrequencySelection(trackId);
    m_frequencySelection.setFrequencyRange(frequency, frequency, m_config->scale());
    m_frequencySelectionChanged.send(false);

    // Since both start and end frequency are equal, use any of start or end frequency handle.
    return m_startFrequencyHandle;
}

FrequencySelection FrequencySelectionController::frequencySelection() const
{
    return m_frequencySelection;
}

void FrequencySelectionController::setFrequencySelection(FrequencySelection frequencySelection, bool complete)
{
    const auto m_config = spectrogramService()->trackSpectrogramConfiguration(frequencySelection.trackId);
    if (!m_config) {
        return;
    }

    const auto startFrequency = std::max(frequencySelection.startFrequency(), 0.0);
    const auto endFrequency
        = std::min(frequencySelection.endFrequency(), spectrogramService()->frequencyHardMaximum(frequencySelection.trackId));
    frequencySelection.setFrequencyRange(startFrequency, endFrequency, m_config->scale());

    if (m_frequencySelection != frequencySelection) {
        m_frequencySelection = frequencySelection;
    }

    // Send even if not changed because the `complete` flag might be different.
    m_frequencySelectionChanged.send(complete);
}

void FrequencySelectionController::setStartFrequency(double frequency, bool complete)
{
    const auto handle = m_startFrequencyHandle;
    if (setEdgeFrequency(frequency, complete, m_startFrequencyHandle)) {
        m_handleDragged.send(handle, complete);
    }
}

void FrequencySelectionController::setEndFrequency(double frequency, bool complete)
{
    const auto handle = m_endFrequencyHandle;
    if (setEdgeFrequency(frequency, complete, m_endFrequencyHandle)) {
        m_handleDragged.send(handle, complete);
    }
}

bool FrequencySelectionController::setEdgeFrequency(double frequency, bool complete, uintptr_t handle)
{
    IF_ASSERT_FAILED(m_config) {
        return false;
    }

    if (frequency != SelectionInfo::UndefinedFrequency) {
        frequency = std::clamp(frequency, 0.0, spectrogramService()->frequencyHardMaximum(m_frequencySelection.trackId));
    }

    const auto referenceFrequency
        = (handle == m_startFrequencyHandle) ? m_frequencySelection.startFrequency() : m_frequencySelection.endFrequency();
    if (muse::is_equal(frequency, referenceFrequency)) {
        // Send even if not changed because the `complete` flag might be different.
        m_frequencySelectionChanged.send(complete);
        return true;
    }

    if (frequency == SelectionInfo::UndefinedFrequency) {
        *reinterpret_cast<double*>(handle) = frequency;
    } else if (handle == m_startFrequencyHandle) {
        m_frequencySelection.setFrequencyRange(frequency, m_frequencySelection.endFrequency(), m_config->scale());
    } else {
        m_frequencySelection.setFrequencyRange(m_frequencySelection.startFrequency(), frequency, m_config->scale());
    }

    m_frequencySelectionChanged.send(complete);

    return true;
}

uintptr_t FrequencySelectionController::startFrequencyHandle() const
{
    return m_startFrequencyHandle;
}

uintptr_t FrequencySelectionController::endFrequencyHandle() const
{
    return m_endFrequencyHandle;
}

void FrequencySelectionController::setHandleFrequency(double frequency, bool complete, uintptr_t handle)
{
    if (m_frequencySelection.trackId == -1) {
        return;
    }

    if (handle == 0) {
        setHandleFrequency(frequency, complete, m_startFrequencyHandle);
        setHandleFrequency(frequency, complete, m_endFrequencyHandle);
        return;
    }

    if ((handle == m_startFrequencyHandle && m_frequencySelection.endFrequency() != SelectionInfo::UndefinedFrequency
         && frequency > m_frequencySelection.endFrequency())) {
        // end frequency becomes start frequency
        setStartFrequency(m_frequencySelection.endFrequency(), complete);
        std::swap(m_startFrequencyHandle, m_endFrequencyHandle);
    } else if (handle == m_endFrequencyHandle && m_frequencySelection.startFrequency() != SelectionInfo::UndefinedFrequency
               && frequency < m_frequencySelection.startFrequency()) {
        // start frequency becomes end frequency
        setEndFrequency(m_frequencySelection.startFrequency(), complete);
        std::swap(m_startFrequencyHandle, m_endFrequencyHandle);
    }

    if (setEdgeFrequency(frequency, complete, handle)) {
        m_handleDragged.send(handle, complete);
    }
}

double FrequencySelectionController::handleFrequency(uintptr_t handle) const
{
    if (handle == m_startFrequencyHandle) {
        return m_frequencySelection.startFrequency();
    } else if (handle == m_endFrequencyHandle) {
        return m_frequencySelection.endFrequency();
    } else if (handle == m_centerFrequencyHandle) {
        return m_frequencySelection.centerFrequency();
    } else {
        assert(false);
        LOGW() << "Unknown handle: " << handle;
        return SelectionInfo::UndefinedFrequency;
    }
}

void FrequencySelectionController::setCenterFrequency(double newCenterFrequency, bool complete)
{
    IF_ASSERT_FAILED(m_frequencySelection.isValid() && m_config) {
        // Expecting center frequency to be changed only when there is already a frequency selection for the track.
        return;
    }

    muse::Defer maybeResetDragStartFrequencyRange([this, complete] {
        if (complete) {
            m_dragStartFrequencyRange.reset();
        }
    });

    if (muse::is_equal(newCenterFrequency, m_frequencySelection.centerFrequency())) {
        // Send even if not changed because the `complete` flag might be different.
        m_handleDragged.send(m_centerFrequencyHandle, complete);
        return;
    }

    NumberScale numberScale(m_config->scale(), m_config->minFreq(), m_config->maxFreq());
    const auto centerFrequencyPos = numberScale.valueToPosition(newCenterFrequency);

    const auto startFreqPos = numberScale.valueToPosition(m_frequencySelection.startFrequency());
    const auto endFreqPos = numberScale.valueToPosition(m_frequencySelection.endFrequency());

    if (!m_dragStartFrequencyRange.has_value()) {
        m_dragStartFrequencyRange = endFreqPos - startFreqPos;
    }

    auto newStartFreqPos = centerFrequencyPos - *m_dragStartFrequencyRange / 2;
    auto newEndFreqPos = centerFrequencyPos + *m_dragStartFrequencyRange / 2;

    // NumberScale normalized position to [0, 1]...
    if (newEndFreqPos > 1) {
        const auto delta = newEndFreqPos - 1;
        newStartFreqPos = std::min<double>(newStartFreqPos + delta, 1);
        newEndFreqPos = 1;
    } else if (newStartFreqPos < 0) {
        const auto delta = -newStartFreqPos;
        newEndFreqPos = std::max<double>(newEndFreqPos - delta, 0);
        newStartFreqPos = 0;
    }

    const auto newStartFreq = numberScale.positionToValue(newStartFreqPos);
    const auto newEndFreq = numberScale.positionToValue(newEndFreqPos);

    setEdgeFrequency(newStartFreq, complete, m_startFrequencyHandle);
    setEdgeFrequency(newEndFreq, complete, m_endFrequencyHandle);
    m_handleDragged.send(m_centerFrequencyHandle, complete);
}

void FrequencySelectionController::resetFrequencySelection()
{
    m_frequencySelection = FrequencySelection{};
    m_frequencySelectionChanged.send(true);
}

muse::async::Channel<bool> FrequencySelectionController::frequencySelectionChanged() const
{
    return m_frequencySelectionChanged;
}

muse::async::Channel<uintptr_t, bool> FrequencySelectionController::handleDragged() const
{
    return m_handleDragged;
}
}
