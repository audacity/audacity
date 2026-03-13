/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "ifrequencyselectioncontroller.h"
#include "ispectrogramservice.h"

#include <unordered_set>

namespace au::spectrogram {
class FrequencySelectionController : public IFrequencySelectionController, public muse::Injectable
{
    muse::Inject<ISpectrogramService> spectrogramService{ this };

public:
    FrequencySelectionController(const muse::modularity::ContextPtr& ctx);

    spectrogram::FrequencySelection frequencySelection() const override;
    bool showsSpectrogram(int trackId) const override;
    void setShowsSpectrogram(int trackId, bool value) override;
    void setFrequencySelection(spectrogram::FrequencySelection, bool complete) override;
    void resetFrequencySelection() override;
    muse::async::Channel<int /*track id*/, bool /*complete*/> frequencySelectionChanged() const override;

private:
    FrequencySelection m_frequencySelection;
    muse::async::Channel<int, bool> m_frequencySelectionChanged;
    std::unordered_set<int> m_tracksWithSpectrogramShown;
};
}
