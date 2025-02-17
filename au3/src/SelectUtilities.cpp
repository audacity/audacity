/**********************************************************************

 Audacity: A Digital Audio Editor

 SelectUtilities.cpp

 Paul Licameli split from SelectMenus.cpp

 **********************************************************************/

#include "SelectUtilities.h"

#include <wx/frame.h>

#include "AudacityMessageBox.h"
#include "AudioIO.h"
#include "CommonCommandFlags.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "ProjectNumericFormats.h"
#include "ProjectWindows.h"
#include "ProjectRate.h"
#include "SelectionState.h"
#include "SyncLock.h"
#include "TimeDialog.h"
#include "TrackFocus.h"
#include "TrackPanel.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

#include "CommandManager.h"

namespace {
// Temporal selection (not TimeTrack selection)
// potentially for all wave tracks.
void DoSelectTimeAndAudioTracks(
    AudacityProject& project, bool bAllTime, bool bAllTracks)
{
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (bAllTime) {
        selectedRegion.setTimes(tracks.GetStartTime(), tracks.GetEndTime());
    }

    if (bAllTracks) {
        // Unselect all tracks before selecting audio.
        for (auto t : tracks) {
            t->SetSelected(false);
        }
        for (auto t : tracks.Any<WaveTrack>()) {
            t->SetSelected(true);
        }
        ProjectHistory::Get(project).ModifyState(false);
    }
}
}
namespace SelectUtilities {
void DoSelectTimeAndTracks
    (AudacityProject& project, bool bAllTime, bool bAllTracks)
{
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (bAllTime) {
        selectedRegion.setTimes(tracks.GetStartTime(), tracks.GetEndTime());
    }

    if (bAllTracks) {
        for (auto t : tracks) {
            t->SetSelected(true);
        }

        ProjectHistory::Get(project).ModifyState(false);
    }
}

void SelectNone(AudacityProject& project)
{
    auto& tracks = TrackList::Get(project);
    for (auto t : tracks) {
        t->SetSelected(false);
    }

    auto& trackPanel = TrackPanel::Get(project);
    trackPanel.Refresh(false);
}

// Select the full time range, if no
// time range is selected.
void SelectAllIfNone(AudacityProject& project)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto flags = CommandManager::Get(project).GetUpdateFlags();
    if ((flags & EditableTracksSelectedFlag()).none()
        || viewInfo.selectedRegion.isPoint()) {
        DoSelectAllAudio(project);
    }
}

// Select the full time range, if no time range is selected and
// selecting is allowed. Returns "false" selecting not allowed.
bool SelectAllIfNoneAndAllowed(AudacityProject& project)
{
    auto allowed = gPrefs->ReadBool(wxT("/GUI/SelectAllOnNone"), false);
    auto& viewInfo = ViewInfo::Get(project);
    auto flags = CommandManager::Get(project).GetUpdateFlags();

    if ((flags & EditableTracksSelectedFlag()).none()
        || viewInfo.selectedRegion.isPoint()) {
        if (!allowed) {
            return false;
        }
        DoSelectAllAudio(project);
    }
    return true;
}

void DoListSelection(
    AudacityProject& project, Track& t, bool shift, bool ctrl, bool modifyState)
{
    auto& tracks = TrackList::Get(project);
    auto& selectionState = SelectionState::Get(project);
    auto& viewInfo = ViewInfo::Get(project);
    auto& window = GetProjectFrame(project);

    auto isSyncLocked = SyncLockState::Get(project).IsSyncLocked();

    selectionState.HandleListSelection(
        tracks, viewInfo, t,
        shift, ctrl, isSyncLocked);

    if (!ctrl) {
        TrackFocus::Get(project).Set(&t);
    }
    window.Refresh(false);
    if (modifyState) {
        ProjectHistory::Get(project).ModifyState(true);
    }
}

void DoSelectAll(AudacityProject& project)
{
    DoSelectTimeAndTracks(project, true, true);
}

void DoSelectAllAudio(AudacityProject& project)
{
    DoSelectTimeAndAudioTracks(project, true, true);
}

// This function selects all tracks if no tracks selected,
// and all time if no time selected.
// There is an argument for making it just count wave tracks,
// However you could then not select a label and cut it,
// without this function selecting all tracks.
void DoSelectSomething(AudacityProject& project)
{
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    bool bTime = selectedRegion.isPoint();
    bool bTracks = tracks.Selected().empty();

    if (bTime || bTracks) {
        DoSelectTimeAndTracks(project, bTime, bTracks);
    }
}

void ActivatePlayRegion(AudacityProject& project)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& playRegion = viewInfo.playRegion;
    playRegion.SetActive(true);
    if (playRegion.Empty()) {
        auto& selectedRegion = viewInfo.selectedRegion;
        if (!selectedRegion.isPoint()) {
            playRegion.SetTimes(selectedRegion.t0(), selectedRegion.t1());
        } else {
            // Arbitrary first four seconds
            playRegion.SetTimes(0.0, 4.0);
        }
    }

    // Ensure the proper state of looping in the menu
    CommandManager::Get(project).UpdateCheckmarks();
}

void InactivatePlayRegion(AudacityProject& project)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& playRegion = viewInfo.playRegion;
    auto& selectedRegion = viewInfo.selectedRegion;
    // Set only the times that are fetched by the playback engine, but not
    // the last-active times that are used for display.
    playRegion.SetActive(false);
    playRegion.SetTimes(selectedRegion.t0(), selectedRegion.t1());

    // Ensure the proper state of looping in the menu
    CommandManager::Get(project).UpdateCheckmarks();
}

void TogglePlayRegion(AudacityProject& project)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& playRegion = viewInfo.playRegion;
    if (playRegion.Active()) {
        InactivatePlayRegion(project);
    } else {
        ActivatePlayRegion(project);
    }
}

void ClearPlayRegion(AudacityProject& project)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& playRegion = viewInfo.playRegion;
    playRegion.Clear();

    if (playRegion.Active()) {
        InactivatePlayRegion(project);
    }
}

void SetPlayRegionToSelection(AudacityProject& project)
{
    auto& viewInfo = ViewInfo::Get(project);
    auto& playRegion = viewInfo.playRegion;
    auto& selectedRegion = viewInfo.selectedRegion;
    playRegion.SetAllTimes(selectedRegion.t0(), selectedRegion.t1());
    if (!playRegion.Empty()) {
        ActivatePlayRegion(project);
    }
}

void OnSetRegion(AudacityProject& project,
                 bool left, bool selection, const TranslatableString& dialogTitle)
{
    auto token = ProjectAudioIO::Get(project).GetAudioIOToken();
    auto& viewInfo = ViewInfo::Get(project);
    auto& playRegion = viewInfo.playRegion;
    auto& selectedRegion = viewInfo.selectedRegion;
    const auto& formats = ProjectNumericFormats::Get(project);
    auto& window = GetProjectFrame(project);

    const auto getValue = [&]() -> double {
        if (selection) {
            if (left) {
                return selectedRegion.t0();
            } else {
                return selectedRegion.t1();
            }
        } else {
            if (left) {
                return playRegion.GetStart();
            } else {
                return playRegion.GetEnd();
            }
        }
    };

    const auto setValue = [&](double value){
        if (selection) {
            if (left) {
                selectedRegion.setT0(value, false);
            } else {
                selectedRegion.setT1(value, false);
            }
        } else {
            if (left) {
                playRegion.SetStart(value);
            } else {
                playRegion.SetEnd(value);
            }
        }
    };

    bool bSelChanged = false;
    auto gAudioIO = AudioIO::Get();
    if ((token > 0) && gAudioIO->IsStreamActive(token)) {
        double indicator = gAudioIO->GetStreamTime();
        setValue(indicator);
        bSelChanged = true;
    } else {
        auto fmt = formats.GetSelectionFormat();

        TimeDialog dlg(&window, dialogTitle,
                       fmt, project, getValue(), XO("Position"));

        if (wxID_OK == dlg.ShowModal()) {
            //Get the value from the dialog
            setValue(std::max(0.0, dlg.GetTimeValue()));
            bSelChanged = true;
        }
    }

    if (bSelChanged) {
        ProjectHistory::Get(project).ModifyState(false);
    }
}
}
