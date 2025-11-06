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

#include "au3trackdata.h"

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

    int64_t newLabelId = labelTrack->AddLabel(selectedRegion, title);
    const auto& newLabel = DomAccessor::findLabel(labelTrack, newLabelId);

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutLabelAdded(DomConverter::label(labelTrack, newLabel));
    }

    selectionController()->setSelectedLabels({ { labelTrack->GetId(), newLabel->GetId() } });

    return true;
}

bool Au3LabelsInteraction::changeLabelTitle(const LabelKey& labelKey, const muse::String& title)
{
    auto& project = projectRef();
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(project, Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return false;
    }

    Au3Label* label = DomAccessor::findLabel(labelTrack, labelKey.itemId);
    IF_ASSERT_FAILED(label) {
        return false;
    }

    label->title = wxFromString(title);

    LOGD() << "changed title of label: " << labelKey.itemId << ", track: " << labelKey.trackId;

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, label));
    }

    return true;
}

bool Au3LabelsInteraction::removeLabel(const LabelKey& labelKey)
{
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return false;
    }

    int labelIndex = labelTrack->GetLabelIndex(labelKey.itemId);
    if (labelIndex == -1) {
        return false;
    }

    labelTrack->DeleteLabel(labelIndex);

    LOGD() << "deleted label: " << labelKey.itemId << ", track: " << labelKey.trackId;

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutTrackChanged(DomConverter::track(labelTrack));
    }

    return true;
}

bool Au3LabelsInteraction::removeLabels(const LabelKeyList& labelKeys)
{
    if (labelKeys.empty()) {
        return false;
    }

    // Group labels by track
    std::map<TrackId, std::vector<int64_t> > labelsByTrack;
    for (const auto& labelKey : labelKeys) {
        labelsByTrack[labelKey.trackId].push_back(labelKey.itemId);
    }

    for (auto& [trackId, labelIds] : labelsByTrack) {
        Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(trackId));
        IF_ASSERT_FAILED(labelTrack) {
            continue;
        }

        // Convert label IDs to indices
        std::vector<int> indices;
        for (int64_t labelId : labelIds) {
            int index = labelTrack->GetLabelIndex(labelId);
            if (index >= 0) {
                indices.push_back(index);
            }
        }

        // Sort indices in descending order to delete from highest to lowest
        std::sort(indices.begin(), indices.end(), std::greater<int>());

        for (int index : indices) {
            labelTrack->DeleteLabel(index);
            LOGD() << "deleted label at index: " << index << ", track: " << trackId;
        }

        const auto prj = globalContext()->currentTrackeditProject();
        if (prj) {
            prj->notifyAboutTrackChanged(DomConverter::track(labelTrack));
        }
    }

    return true;
}

ITrackDataPtr Au3LabelsInteraction::cutLabel(const LabelKey& labelKey)
{
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return nullptr;
    }

    const Au3Label* label = DomAccessor::findLabel(labelTrack, labelKey.itemId);
    if (!label) {
        return nullptr;
    }

    const Au3Label labelCopy = *label;

    constexpr bool moveClips = true;
    auto track = labelTrack->Cut(label->getT0(), label->getT1(), moveClips);
    const auto data = std::make_shared<Au3TrackData>(std::move(track));

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    prj->notifyAboutLabelRemoved(DomConverter::label(labelTrack, &labelCopy));
    prj->notifyAboutTrackChanged(DomConverter::track(labelTrack));

    return data;
}

ITrackDataPtr Au3LabelsInteraction::copyLabel(const LabelKey& labelKey)
{
    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return nullptr;
    }

    const Au3Label* label = DomAccessor::findLabel(labelTrack, labelKey.itemId);
    if (!label) {
        return nullptr;
    }

    auto track = labelTrack->Copy(label->getT0(), label->getT1());

    return std::make_shared<Au3TrackData>(std::move(track));
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

        int labelIndex = labelTrack->GetLabelIndex(selectedLabel.itemId);
        IF_ASSERT_FAILED(labelIndex >= 0) {
            continue;
        }

        const auto& au3labels = labelTrack->GetLabels();
        Au3Label au3Label = au3labels[labelIndex];

        // Calculate new times
        double newT0 = std::max(0.0, au3Label.getT0() + timePositionOffset);
        double newT1 = std::max(0.0, au3Label.getT1() + timePositionOffset);

        // Update the label with new times
        au3Label.selectedRegion.setTimes(newT0, newT1);
        labelTrack->SetLabel(labelIndex, au3Label);

        if (prj) {
            prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, DomAccessor::findLabel(labelTrack, au3Label.GetId())));
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

    int labelIndex = labelTrack->GetLabelIndex(labelKey.itemId);
    if (labelIndex == -1) {
        return false;
    }

    const auto& au3labels = labelTrack->GetLabels();
    Au3Label au3Label = au3labels[labelIndex];

    // Update the label with new start time
    au3Label.selectedRegion.setTimes(newStartTime, au3Label.getT1());
    labelTrack->SetLabel(labelIndex, au3Label);

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, DomAccessor::findLabel(labelTrack, au3Label.GetId())));
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

    int labelIndex = labelTrack->GetLabelIndex(labelKey.itemId);
    if (labelIndex == -1) {
        return false;
    }

    const auto& au3labels = labelTrack->GetLabels();
    Au3Label au3Label = au3labels[labelIndex];

    // Update the label with new end time
    au3Label.selectedRegion.setTimes(au3Label.getT0(), newEndTime);
    labelTrack->SetLabel(labelIndex, au3Label);

    const auto prj = globalContext()->currentTrackeditProject();
    if (prj) {
        prj->notifyAboutLabelChanged(DomConverter::label(labelTrack, DomAccessor::findLabel(labelTrack, au3Label.GetId())));
    }

    return true;
}

muse::Progress Au3LabelsInteraction::progress() const
{
    return m_progress;
}
