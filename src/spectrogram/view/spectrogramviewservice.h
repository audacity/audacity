/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramviewservice.h"
#include "ifrequencyselectioncontroller.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

namespace au::spectrogram {
class SpectrogramViewService final : public ISpectrogramViewService, public muse::Injectable, public muse::async::Asyncable
{
    muse::Inject<IFrequencySelectionController> frequencySelectionController { this };

public:
    SpectrogramViewService(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}
    ~SpectrogramViewService() override = default;

    void init();

    double rulerGuideFrequency(int trackId) const override;
    void setRulerGuideFrequency(int trackId, double frequency) override;
    muse::async::Channel<int> rulerGuideFrequencyChanged() const override;

private:
    void doSetRulerGuideFrequency(int trackId, double frequency);

    struct RulerGuide {
        int trackId = -1;
        double frequency = -1;
    };

    RulerGuide m_rulerGuide;
    muse::async::Channel<int> m_rulerGuideFrequencyChanged;
    bool m_handleBeingDragged = false;
};
}
