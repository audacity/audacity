/*
 * Audacity: A Digital Audio Editor
 */
#include "au3frequencyselectionrestorer.h"

#include "au3-time-frequency-selection/ViewInfo.h"
#include "au3wrap/internal/domaccessor.h"

namespace au::spectrogram {
void FrequencySelectionRestorer::storeFrequencySelectionState(const FrequencySelection& selection) const
{
    const auto project = au3Project();
    if (!project) {
        return;
    }

    auto& selectedRegion = ::ViewInfo::Get(*project).selectedRegion;
    selectedRegion.setF0(selection.startFrequency());
    selectedRegion.setF1(selection.endFrequency());
}

FrequencySelection FrequencySelectionRestorer::loadFrequencySelectionState() const
{
    const auto project = au3Project();
    if (!project) {
        return {};
    }

    auto& selectedRegion = ::ViewInfo::Get(*project).selectedRegion;
    const auto restoredFocusedTrack = au3::DomAccessor::findFocusedTrack(*project);

    if (restoredFocusedTrack == -1 || selectedRegion.f0() == -1 || selectedRegion.f1() == -1) {
        return {};
    }

    const auto config = spectrogramService()->trackSpectrogramConfiguration(restoredFocusedTrack);
    if (!config) {
        return {};
    }

    spectrogram::FrequencySelection selection(restoredFocusedTrack);
    selection.setFrequencyRange(selectedRegion.f0(), selectedRegion.f1(), config->scale());
    return selection;
}
}
