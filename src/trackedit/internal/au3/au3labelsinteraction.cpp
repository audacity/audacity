/*
 * Audacity: A Digital Audio Editor
 */

#include "au3labelsinteraction.h"

#include <map>
#include <algorithm>

#include "libraries/lib-label-track/LabelTrack.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "defer.h"
#include "log.h"

using namespace au::trackedit;
using namespace au::au3;

Au3LabelsInteraction::Au3LabelsInteraction()
{
    m_progress.setMaxNumIncrements(200);
}

au::au3::Au3Project& Au3LabelsInteraction::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

au::context::IPlaybackStatePtr Au3LabelsInteraction::playbackState() const
{
    return globalContext()->playbackState();
}

bool Au3LabelsInteraction::addLabelToSelection()
{
    auto& project = projectRef();
    auto& tracks = Au3TrackList::Get(project);

    Au3LabelTrack* labelTrack = nullptr;

    const auto focusedTrackId = selectionController()->focusedTrack();
    if (focusedTrackId > 0) {
        Au3Track* focusedAu3Track = DomAccessor::findTrack(project, Au3TrackId(focusedTrackId));
        if (focusedAu3Track) {
            labelTrack = dynamic_cast<Au3LabelTrack*>(focusedAu3Track);
        }
    }

    // If the focused track is not a label track, search for any existing label track
    if (!labelTrack) {
        for (auto lt : tracks.Any<Au3LabelTrack>()) {
            labelTrack = lt;
            break;
        }
    }

    // If no label track exists, create a new one
    if (!labelTrack) {
        labelTrack = ::LabelTrack::Create(tracks);

        const auto prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutTrackAdded(DomConverter::labelTrack(labelTrack));
    }

    wxString title = wxEmptyString;
    SelectedRegion selectedRegion;
    selectedRegion.setTimes(selectionController()->dataSelectedStartTime(),
                            selectionController()->dataSelectedEndTime());

    int labelIndex = labelTrack->AddLabel(selectedRegion, title);

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        const auto& au3labels = labelTrack->GetLabels();
        if (labelIndex >= 0 && labelIndex < static_cast<int>(au3labels.size())) {
            prj->notifyAboutLabelAdded(DomConverter::label(labelTrack, labelIndex, au3labels[labelIndex]));
        }
    }

    selectionController()->setFocusedTrack(labelTrack->GetId());

    return true;
}

bool Au3LabelsInteraction::changeLabelTitle(const LabelKey& labelKey, const muse::String& title)
{
    auto& project = projectRef();
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(project, Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return false;
    }

    const auto& au3labels = labelTrack->GetLabels();
    size_t labelIndex = static_cast<size_t>(labelKey.itemId);

    IF_ASSERT_FAILED(labelIndex < au3labels.size()) {
        return false;
    }

    Au3Label au3Label = au3labels[labelIndex];
    au3Label.title = wxFromString(title);
    labelTrack->SetLabel(labelIndex, au3Label);

    LOGD() << "changed title of label: " << labelKey.itemId << ", track: " << labelKey.trackId;

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, labelIndex, labelTrack->GetLabels()[labelIndex]));
    }

    return true;
}

bool Au3LabelsInteraction::removeLabel(const LabelKey& labelKey)
{
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return false;
    }

    const auto& au3labels = labelTrack->GetLabels();
    size_t labelIndex = static_cast<size_t>(labelKey.itemId);

    IF_ASSERT_FAILED(labelIndex < au3labels.size()) {
        return false;
    }

    labelTrack->DeleteLabel(labelIndex);

    LOGD() << "deleted label: " << labelKey.itemId << ", track: " << labelKey.trackId;

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutTrackChanged(DomConverter::track(labelTrack));
    }

    projectHistory()->pushHistoryState("Delete label", "Delete");

    return true;
}

bool Au3LabelsInteraction::removeLabels(const LabelKeyList& labelKeys)
{
    if (labelKeys.empty()) {
        return false;
    }

    // Group labels by track
    std::map<TrackId, std::vector<size_t>> labelsByTrack;
    for (const auto& labelKey : labelKeys) {
        labelsByTrack[labelKey.trackId].push_back(static_cast<size_t>(labelKey.itemId));
    }

    // Delete labels from each track, starting from the highest index to avoid index shifts
    for (auto& [trackId, indices] : labelsByTrack) {
        Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(labelTrack) {
            continue;
        }

        // Sort indices in descending order to delete from highest to lowest
        std::sort(indices.begin(), indices.end(), std::greater<size_t>());

        const auto& au3labels = labelTrack->GetLabels();
        for (size_t index : indices) {
            if (index < au3labels.size()) {
                labelTrack->DeleteLabel(index);
                LOGD() << "deleted label: " << index << ", track: " << trackId;
            }
        }

        const auto prj = globalContext()->currentTrackeditProject();
        if (prj) {
            prj->notifyAboutTrackChanged(DomConverter::track(labelTrack));
        }
    }

    projectHistory()->pushHistoryState("Delete labels", "Delete");

    return true;
}

bool Au3LabelsInteraction::moveLabels(secs_t timePositionOffset, bool completed)
{
    if (muse::RealIsEqual(timePositionOffset, 0.0)) {
        return true;
    }

    //! NOTE: cannot start moving until previous move is handled
    if (m_busy) {
        return false;
    }
    m_busy = true;

    DEFER {
        m_busy = false;
    };

    const trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    auto selectedLabels = selectionController()->selectedLabels();
    if (selectedLabels.empty()) {
        return false;
    }

    for (const auto& selectedLabel : selectedLabels) {
        Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(selectedLabel.trackId));
        IF_ASSERT_FAILED(labelTrack) {
            continue;
        }

        const auto& au3labels = labelTrack->GetLabels();
        size_t labelIndex = static_cast<size_t>(selectedLabel.itemId);

        IF_ASSERT_FAILED(labelIndex < au3labels.size()) {
            continue;
        }

        Au3Label au3Label = au3labels[labelIndex];

        // Calculate new times
        double newT0 = std::max(0.0, au3Label.getT0() + timePositionOffset);
        double newT1 = std::max(0.0, au3Label.getT1() + timePositionOffset);

        // Update the label with new times
        au3Label.selectedRegion.setTimes(newT0, newT1);
        labelTrack->SetLabel(labelIndex, au3Label);

        if (prj) {
            prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, labelIndex, labelTrack->GetLabels()[labelIndex]));
        }
    }

    return true;
}

bool Au3LabelsInteraction::stretchLabelLeft(const LabelKey& labelKey, secs_t newStartTime, bool completed)
{
    //! NOTE: cannot start stretching until previous stretch is handled
    if (m_busy) {
        return false;
    }
    m_busy = true;

    DEFER {
        m_busy = false;
    };

    auto& project = projectRef();
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(project, Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return false;
    }

    const auto& au3labels = labelTrack->GetLabels();
    size_t labelIndex = static_cast<size_t>(labelKey.itemId);

    IF_ASSERT_FAILED(labelIndex < au3labels.size()) {
        return false;
    }

    Au3Label au3Label = au3labels[labelIndex];

    // Update the label with new start time
    au3Label.selectedRegion.setTimes(newStartTime, au3Label.getT1());
    labelTrack->SetLabel(labelIndex, au3Label);

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, labelIndex, labelTrack->GetLabels()[labelIndex]));
    }

    return true;
}

bool Au3LabelsInteraction::stretchLabelRight(const LabelKey& labelKey, secs_t newEndTime, bool completed)
{
    //! NOTE: cannot start stretching until previous stretch is handled
    if (m_busy) {
        return false;
    }
    m_busy = true;

    DEFER {
        m_busy = false;
    };

    auto& project = projectRef();
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(project, Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return false;
    }

    const auto& au3labels = labelTrack->GetLabels();
    size_t labelIndex = static_cast<size_t>(labelKey.itemId);

    IF_ASSERT_FAILED(labelIndex < au3labels.size()) {
        return false;
    }

    Au3Label au3Label = au3labels[labelIndex];

    // Update the label with new end time
    au3Label.selectedRegion.setTimes(au3Label.getT0(), newEndTime);
    labelTrack->SetLabel(labelIndex, au3Label);

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, labelIndex, labelTrack->GetLabels()[labelIndex]));
    }

    return true;
}

muse::Progress Au3LabelsInteraction::progress() const
{
    return m_progress;
}
