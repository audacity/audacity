/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "ifrequencyselectioncontroller.h"
#include "ispectrogramservice.h"

#include "framework/global/async/asyncable.h"

#include <unordered_set>

namespace au::spectrogram {
class IFrequencySelectionRestorer;

class FrequencySelectionController : public IFrequencySelectionController, public muse::Injectable, public muse::async::Asyncable
{
    muse::Inject<ISpectrogramService> spectrogramService{ this };

public:
    FrequencySelectionController(const muse::modularity::ContextPtr& ctx,
                                 std::unique_ptr<IFrequencySelectionRestorer> frequencySelectionRestorer);

    spectrogram::FrequencySelection frequencySelection() const override;
    bool showsSpectrogram(int trackId) const override;
    void setShowsSpectrogram(int trackId, bool value) override;
    void setFrequencySelection(spectrogram::FrequencySelection, bool complete) override;
    void resetFrequencySelection() override;
    muse::async::Channel<int /*track id*/, bool /*complete*/> frequencySelectionChanged() const override;

    void restoreFrequencySelection() override;

private:
    FrequencySelection m_frequencySelection;
    muse::async::Channel<int, bool> m_frequencySelectionChanged;
    std::unordered_set<int> m_tracksWithSpectrogramShown;
    const std::unique_ptr<IFrequencySelectionRestorer> m_frequencySelectionRestorer;
};
}
