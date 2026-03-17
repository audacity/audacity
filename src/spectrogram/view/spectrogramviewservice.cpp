/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramviewservice.h"

#include "spectrogramtypes.h"
#include "types/number.h"

#include <optional>

namespace au::spectrogram {
void SpectrogramViewService::init()
{
    frequencySelectionController()->handleDragged().onReceive(this, [this](uintptr_t handle, bool complete) {
        m_handleBeingDragged = !complete;
        doSetRulerGuideFrequency(m_rulerGuide.trackId, frequencySelectionController()->handleFrequency(handle));
    });
}

double SpectrogramViewService::rulerGuideFrequency(int trackId) const
{
    return m_rulerGuide.trackId == trackId ? m_rulerGuide.frequency : SelectionInfo::UndefinedFrequency;
}

void SpectrogramViewService::setRulerGuideFrequency(int trackId, double frequency)
{
    if (!m_handleBeingDragged) {
        doSetRulerGuideFrequency(trackId, frequency);
    }
}

void SpectrogramViewService::doSetRulerGuideFrequency(int trackId, double frequency)
{
    if (trackId == m_rulerGuide.trackId && muse::is_equal(frequency, m_rulerGuide.frequency)) {
        return;
    }

    if (trackId == m_rulerGuide.trackId && frequency == SelectionInfo::UndefinedFrequency) {
        m_rulerGuide = {};
        m_rulerGuideFrequencyChanged.send(trackId);
        return;
    }

    if (frequency == SelectionInfo::UndefinedFrequency) {
        // ignore
        return;
    }

    const int previousTrackId = m_rulerGuide.trackId;

    m_rulerGuide = { trackId, frequency };
    m_rulerGuideFrequencyChanged.send(trackId);

    if (previousTrackId != trackId) {
        m_rulerGuideFrequencyChanged.send(previousTrackId);
    }
}

muse::async::Channel<int> SpectrogramViewService::rulerGuideFrequencyChanged() const
{
    return m_rulerGuideFrequencyChanged;
}
}
