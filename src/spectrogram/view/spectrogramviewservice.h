/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramviewservice.h"

namespace au::spectrogram {
class SpectrogramViewService final : public ISpectrogramViewService
{
public:
    ~SpectrogramViewService() override = default;

    double rulerGuideFrequency(int trackId) const override;
    void setRulerGuideFrequency(int trackId, double frequency) override;
    muse::async::Channel<int> rulerGuideFrequencyChanged() const override;

private:
    RulerGuide m_rulerGuide;
    muse::async::Channel<int> m_rulerGuideFrequencyChanged;
};
}
