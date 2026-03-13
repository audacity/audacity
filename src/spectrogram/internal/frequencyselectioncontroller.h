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
    void setStartFrequency(int trackId, double frequency, bool complete) override;
    void setEndFrequency(int trackId, double frequency, bool complete) override;
    void setCenterFrequency(int trackId, double frequency) override;
    void resetFrequencySelection() override;
    muse::async::Channel<int /*track id*/, std::optional<bool> /*complete*/> frequencySelectionChanged() const override;
    muse::async::Channel<int /*track ID*/, std::optional<bool> /* complete */> startFrequencyChanged() const override;
    muse::async::Channel<int /*track ID*/, std::optional<bool> /* complete */> endFrequencyChanged() const override;
    muse::async::Channel<int /*track ID*/> centerFrequencyChanged() const override;

private:
    void setEdgeFrequency(int trackId, double frequency, const std::optional<bool>& complete, bool isStart);
    void notifyAboutStartFrequencyChanged(int trackId, const std::optional<bool>& complete);
    void notifyAboutEndFrequencyChanged(int trackId, const std::optional<bool>& complete);

    FrequencySelection m_frequencySelection;
    muse::async::Channel<int, std::optional<bool> > m_frequencySelectionChanged;
    muse::async::Channel<int, std::optional<bool> > m_startFrequencyChanged;
    muse::async::Channel<int, std::optional<bool> > m_endFrequencyChanged;
    muse::async::Channel<int> m_centerFrequencyChanged;
    std::unordered_set<int> m_tracksWithSpectrogramShown;
};
}
