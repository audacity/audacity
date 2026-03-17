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

    void init();

    FrequencySelection frequencySelection() const override;
    bool showsSpectrogram(int trackId) const override;
    void setShowsSpectrogram(int trackId, bool value) override;

    uintptr_t beginSelection(int trackId, double frequency) override;
    void setCenterFrequency(double frequency, bool complete) override;
    void resetFrequencySelection() override;

    muse::async::Channel<bool /*complete*/> frequencySelectionChanged() const override;
    muse::async::Channel<uintptr_t, bool /* complete */> handleDragged() const override;

    uintptr_t startFrequencyHandle() const override;
    uintptr_t endFrequencyHandle() const override;
    void setHandleFrequency(double frequency, bool complete, uintptr_t handle) override;
    double handleFrequency(uintptr_t handle) const override;

    void restoreFrequencySelection() override;

private:
    void setFrequencySelection(FrequencySelection, bool complete);
    void setStartFrequency(double frequency, bool complete);
    void setEndFrequency(double frequency, bool complete);
    bool setEdgeFrequency(double frequency, bool complete, uintptr_t handle);

    ITrackSpectrogramConfigurationPtr m_config;
    FrequencySelection m_frequencySelection;
    uintptr_t m_startFrequencyHandle = reinterpret_cast<uintptr_t>(&m_frequencySelection.m_startFrequency);
    uintptr_t m_endFrequencyHandle = reinterpret_cast<uintptr_t>(&m_frequencySelection.m_endFrequency);
    uintptr_t m_centerFrequencyHandle = reinterpret_cast<uintptr_t>(&m_frequencySelection.m_centerFrequency);

    muse::async::Channel<bool> m_frequencySelectionChanged;
    muse::async::Channel<uintptr_t, bool> m_handleDragged;
    std::unordered_set<int> m_tracksWithSpectrogramShown;
    const std::unique_ptr<IFrequencySelectionRestorer> m_frequencySelectionRestorer;
};
}
