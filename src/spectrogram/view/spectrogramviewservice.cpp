/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramviewservice.h"

#include "spectrogramtypes.h"
#include "types/number.h"

namespace au::spectrogram {
void SpectrogramViewService::init()
{
    frequencySelectionController()->handleDragged().onReceive(this, [this](uintptr_t handle, bool complete) {
        m_handleBeingDragged = !complete;
        const auto frequency = frequencySelectionController()->handleFrequency(handle);
        doSetRulerGuideFrequency(m_trackId, frequency);
    });
}

double SpectrogramViewService::rulerGuideFrequency(int trackId) const
{
    return m_trackId == trackId ? m_frequency : SelectionInfo::UndefinedFrequency;
}

void SpectrogramViewService::setRulerGuideFrequency(int trackId, double frequency)
{
    if (!m_handleBeingDragged) {
        doSetRulerGuideFrequency(trackId, frequency);
    }
}

void SpectrogramViewService::doSetRulerGuideFrequency(int trackId, double frequency)
{
    if (trackId == m_trackId && muse::is_equal(frequency, m_frequency)) {
        return;
    }

    const int previousTrackId = m_trackId;

    m_trackId = trackId;
    m_frequency = frequency;
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
