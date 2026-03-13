/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramviewservice.h"

#include "framework/global/defer.h"
#include "spectrogramtypes.h"
#include "types/number.h"

#include <optional>

namespace au::spectrogram {
void SpectrogramViewService::init()
{
    frequencySelectionController()->startFrequencyChanged().onReceive(this, [this](int trackId, auto) {
        setRulerGuideFrequency(trackId, frequencySelectionController()->frequencySelection().startFrequency());
    });
    frequencySelectionController()->endFrequencyChanged().onReceive(this, [this](int trackId, auto) {
        setRulerGuideFrequency(trackId, frequencySelectionController()->frequencySelection().endFrequency());
    });
}

double SpectrogramViewService::rulerGuideFrequency(int trackId) const
{
    return m_rulerGuide.trackId == trackId ? m_rulerGuide.frequency : SelectionInfo::UndefinedFrequency;
}

void SpectrogramViewService::setRulerGuideFrequency(int trackId, double frequency)
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
