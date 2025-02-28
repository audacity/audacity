#include "AudioIO.h"
#include "BasicUI.h"
#include "../Clipboard.h"
#include "../CommonCommandFlags.h"
#include "LabelTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"

#include "../SelectUtilities.h"
#include "SyncLock.h"
#include "TrackFocus.h"
#include "../TrackPanel.h"
#include "ViewInfo.h"
#include "Viewport.h"
#include "WaveTrack.h"
#include "TimeStretching.h"
#include "CommandContext.h"
#include "../tracks/labeltrack/ui/LabelTrackView.h"
#include "toolbars/ToolManager.h"

#include <cassert>

using Region = WaveTrack::Region;
using Regions = WaveTrack::Regions;

// private helper classes and functions
namespace {
const ReservedCommandFlag
& LabelsSelectedFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            // At least one label track selected, having at least one label
            // completely within the time selection.
            const auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
            const auto& test = [&]( const LabelTrack* pTrack ){
                const auto& labels = pTrack->GetLabels();
                return std::any_of(labels.begin(), labels.end(),
                                   [&](const LabelStruct& label){
                    return
                        label.getT0() >= selectedRegion.t0()
                        &&
                        label.getT1() <= selectedRegion.t1()
                    ;
                }
                                   );
            };
            auto range = TrackList::Get(project).Selected<const LabelTrack>()
                         + test;
            return !range.empty();
        }
    };
    return flag;
}

//Adds label and returns index of label in labeltrack.
int DoAddLabel(
    AudacityProject& project, const SelectedRegion& region,
    bool preserveFocus = false)
{
    auto& tracks = TrackList::Get(project);
    auto& trackFocus = TrackFocus::Get(project);
    auto& trackPanel = TrackPanel::Get(project);

    wxString title;     // of label

    bool useDialog;
    gPrefs->Read(wxT("/GUI/DialogForNameNewLabel"), &useDialog, false);
    if (useDialog) {
        if (LabelTrackView::DialogForLabelName(
                project, region, wxEmptyString, title) == wxID_CANCEL) {
            return -1;  // index
        }
    }

    // If the focused track is a label track, use that
    const auto pFocusedTrack = trackFocus.Get();

    // Look for a label track at or after the focused track
    auto begin = tracks.begin();
    auto iter = pFocusedTrack
                ? tracks.Find(pFocusedTrack)
                : begin;
    auto lt = *iter.Filter<LabelTrack>();

    // If none found, start a NEW label track and use it
    if (!lt) {
        lt = LabelTrack::Create(tracks);
    }

// LLL: Commented as it seemed a little forceful to remove users
//      selection when adding the label.  This does not happen if
//      you select several tracks and the last one of those is a
//      label track...typing a label will not clear the selections.
//
//   SelectNone();
    lt->SetSelected(true);

    int index;
    if (useDialog) {
        index = lt->AddLabel(region, title);
    } else {
        int focusTrackNumber = -1;
        if (pFocusedTrack && preserveFocus) {
            // Must remember the track to re-focus after finishing a label edit.
            // do NOT identify it by a pointer, which might dangle!  Identify
            // by position.
            focusTrackNumber = std::distance(begin, iter);
        }
        index
            =LabelTrackView::Get(*lt).AddLabel(region, title, focusTrackNumber);
    }

    ProjectHistory::Get(project)
    .PushState(XO("Added label"), XO("Label"));

    if (!useDialog) {
        TrackFocus::Get(project).Set(lt);
        Viewport::Get(project).ShowTrack(*lt);
    }
    trackPanel.SetFocus();

    return index;
}

//get regions selected by selected labels
//removes unnecessary regions, overlapping regions are merged
void GetRegionsByLabel(
    const TrackList& tracks, const SelectedRegion& selectedRegion,
    Regions& regions)
{
    //determine labeled regions
    for (auto lt : tracks.Selected< const LabelTrack >()) {
        for (int i = 0; i < lt->GetNumLabels(); i++) {
            const LabelStruct* ls = lt->GetLabel(i);
            if (ls->selectedRegion.t0() >= selectedRegion.t0()
                && ls->selectedRegion.t1() <= selectedRegion.t1()) {
                regions.push_back(Region(ls->getT0(), ls->getT1()));
            }
        }
    }

    //anything to do ?
    if (regions.size() == 0) {
        return;
    }

    //sort and remove unnecessary regions
    std::sort(regions.begin(), regions.end());
    unsigned int selected = 1;
    while (selected < regions.size())
    {
        const Region& cur = regions.at(selected);
        Region& last = regions.at(selected - 1);
        if (cur.start < last.end) {
            if (cur.end > last.end) {
                last.end = cur.end;
            }
            regions.erase(regions.begin() + selected);
        } else {
            ++selected;
        }
    }
}

auto GetTracksToEditByLabel(const AudacityProject& project, const TrackList& tracks)
{
    const bool syncLocked = SyncLockState::Get(project).IsSyncLocked();
    const bool playableTrackSelected = !tracks.Selected<const PlayableTrack>().empty();

    return [syncLocked, playableTrackSelected](const auto pTrack) {
        // All sync-locked tracks should be edited,
        // if sync lock is enabled regardless of selection
        if (syncLocked) {
            return SyncLock::IsSyncLockSelected(*pTrack);
        }

        // Non-playable tracks should not be edited if sync-lock disabled
        if (dynamic_cast<const PlayableTrack*>(pTrack) == nullptr) {
            return false;
        }

        // If no playable tracks were selected,
        // then all tracks should be edited
        return playableTrackSelected ? pTrack->IsSelected() : true;
    };
}

using EditFunction = std::function<void (Track& track, double, double)>;
using EditFunctionWithProgress
    =std::function<void (Track& track, double, double, ProgressReporter)>;

// Executes the edit function on all selected wave tracks with
// regions specified by selected labels
// If No tracks selected, function is applied on all tracks
// If the function replaces the selection with audio of a different length,
// bSyncLockedTracks should be set true to perform the same action on sync-lock
// selected tracks.
// If `progress` is non-null, `action` is passed a callable `ProgressReporter`.
void EditByLabel(
    AudacityProject& project, TrackList& tracks,
    const SelectedRegion& selectedRegion, EditFunctionWithProgress action,
    ProgressReporter progress)
{
    Regions regions;

    GetRegionsByLabel(tracks, selectedRegion, regions);
    if (regions.empty()) {
        return;
    }

    const auto tracksToEdit = tracks.Any<Track>() + GetTracksToEditByLabel(project, tracks);

    BasicUI::SplitProgress(
        tracksToEdit.begin(), tracksToEdit.end(),
        [&](Track* aTrack, const ProgressReporter& child) {
        // Apply action on tracks starting from labeled regions in the end.
        // This is to correctly perform actions like 'Delete' which collapse
        // the track area.
        BasicUI::SplitProgress(
            regions.rbegin(), regions.rend(),
            [&](const Region& region, const ProgressReporter& grandChild) {
            action(*aTrack, region.start, region.end, grandChild);
        },
            child);
    },
        progress);
}

void EditByLabel(
    AudacityProject& project, TrackList& tracks,
    const SelectedRegion& selectedRegion, EditFunction action)
{
    EditByLabel(
        project, tracks, selectedRegion,
        [&](Track& track, double t0, double t1, ProgressReporter) {
        action(track, t0, t1);
    },
        nullptr);
}

using EditDestFunction
    =std::function<Track::Holder (Track&, double, double)>;

//Executes the edit function on all selected wave tracks with
//regions specified by selected labels
//If No tracks selected, function is applied on all tracks
//Functions copy the edited regions to clipboard, possibly in multiple tracks
//This probably should not be called if *action() changes the timeline, because
// the copy needs to happen by track, and the timeline change by group.
void EditClipboardByLabel(AudacityProject& project,
                          TrackList& tracks, const SelectedRegion& selectedRegion,
                          EditDestFunction action)
{
    Regions regions;

    GetRegionsByLabel(tracks, selectedRegion, regions);
    if (regions.empty()) {
        return;
    }

    const auto tracksToEdit = tracks.Any<Track>() + GetTracksToEditByLabel(project, tracks);

    auto& clipboard = Clipboard::Get();
    clipboard.Clear();

    auto pNewClipboard = TrackList::Create(nullptr);
    auto& newClipboard = *pNewClipboard;

    //Apply action on wavetracks starting from
    //labeled regions in the end. This is to correctly perform
    //actions like 'Cut' which collapse the track area.

    for (auto t : tracksToEdit) {
        // These tracks accumulate the needed clips, right to left:
        Track::Holder merged;
        for (size_t i = regions.size(); i--;) {
            const Region& region = regions.at(i);
            if (auto dest = action(*t, region.start, region.end)) {
                if (!merged) {
                    merged = dest;
                } else {
                    // Paste to the beginning; unless this is the first region,
                    // offset the track to account for time between the regions
                    if (i + 1 < regions.size()) {
                        merged->ShiftBy(
                            regions.at(i + 1).start - region.end);
                    }

                    // If GetEditClipsCanMove is not set, we need to manually
                    // shift the merged track by the end time of the dest clip,
                    // to make room for pasting because Paste does not
                    // handle shifting in this case.
                    if (!GetEditClipsCanMove()) {
                        merged->ShiftBy(dest->GetEndTime());
                    }

                    // dest may have a placeholder clip at the end that is
                    // removed when pasting, which is okay because we proceed
                    // right to left.  Any placeholder already in merged is kept.
                    // Only the rightmost placeholder is important in the final
                    // result.
                    merged->Paste(0.0, *dest);
                }
            } else
            // nothing copied but there is a 'region', so the 'region' must
            // be a 'point label' so offset
            if (i + 1 < regions.size() && merged) {
                merged->ShiftBy(regions.at(i + 1).start - region.end);
            }
        }
        if (merged) {
            newClipboard.Add(merged);
        }
    }

    // Survived possibility of exceptions.  Commit changes to the clipboard now.
    clipboard.Assign(std::move(newClipboard),
                     regions.front().start, regions.back().end, project.shared_from_this());
}
}

/// Namespace for functions for Edit Label submenu
namespace {
// Menu handler functions

void OnEditLabels(const CommandContext& context)
{
    auto& project = context.project;
    LabelTrackView::DoEditLabels(project);
}

void OnAddLabel(const CommandContext& context)
{
    auto& project = context.project;
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    DoAddLabel(project, selectedRegion);
}

void OnAddLabelPlaying(const CommandContext& context)
{
    auto& project = context.project;
    auto token = ProjectAudioIO::Get(project).GetAudioIOToken();

    auto gAudioIO = AudioIO::Get();
    if (token > 0
        && gAudioIO->IsStreamActive(token)) {
        double indicator = gAudioIO->GetStreamTime();
        DoAddLabel(project, SelectedRegion(indicator, indicator), true);
    }
}

// Creates a NEW label in each selected label track with text from the system
// clipboard
void OnPasteNewLabel(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& trackPanel = TrackPanel::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    bool bPastedSomething = false;

    {
        auto trackRange = tracks.Selected<const LabelTrack>();
        if (trackRange.empty()) {
            // If there are no selected label tracks, try to choose the first label
            // track after some other selected track
            Track* t = *tracks.Selected().begin()
                       .Filter(&Track::Any)
                       .Filter<LabelTrack>();

            // If no match found, add one
            if (!t) {
                t = LabelTrack::Create(tracks);
            }

            // Select this track so the loop picks it up
            t->SetSelected(true);
        }
    }

    LabelTrack* plt = NULL; // the previous track
    for ( auto lt : tracks.Selected<LabelTrack>()) {
        // Unselect the last label, so we'll have just one active label when
        // we're done
        if (plt) {
            LabelTrackView::Get(*plt).ResetTextSelection();
        }

        // Add a NEW label, paste into it
        // Paul L:  copy whatever defines the selected region, not just times
        auto& view = LabelTrackView::Get(*lt);
        view.AddLabel(selectedRegion);
        if (view.PasteSelectedText(context.project, selectedRegion.t0(),
                                   selectedRegion.t1())) {
            bPastedSomething = true;
        }

        // Set previous track
        plt = lt;
    }

    // plt should point to the last label track pasted to -- ensure it's visible
    // and set focus
    if (plt) {
        TrackFocus::Get(project).Set(plt);
        Viewport::Get(project).ShowTrack(*plt);
        trackPanel.SetFocus();
    }

    if (bPastedSomething) {
        ProjectHistory::Get(project).PushState(
            XO("Pasted from the clipboard"), XO("Paste Text to New Label"));
    }
}

void OnToggleTypeToCreateLabel(const CommandContext& WXUNUSED(context))
{
    bool typeToCreateLabel;
    gPrefs->Read(wxT("/GUI/TypeToCreateLabel"), &typeToCreateLabel, false);
    gPrefs->Write(wxT("/GUI/TypeToCreateLabel"), !typeToCreateLabel);
    gPrefs->Flush();
    ToolManager::ModifyAllProjectToolbarMenus();
}

void OnCutLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    // Because of grouping the copy may need to operate on different tracks than
    // the clear, so we do these actions separately.
    auto copyfunc = [&](Track& track, double t0, double t1) {
        Track::Holder result;
        track.TypeSwitch([&](WaveTrack& wt) { result = wt.Copy(t0, t1); });
        return result;
    };
    EditClipboardByLabel(project, tracks, selectedRegion, copyfunc);

    bool enableCutlines = gPrefs->ReadBool(wxT("/GUI/EnableCutLines"), false);
    auto editfunc = [&](Track& track, double t0, double t1) {
        track.TypeSwitch(
            [&](WaveTrack& t) {
            if (enableCutlines) {
                t.ClearAndAddCutLine(t0, t1);
            } else {
                t.Clear(t0, t1);
            }
        },
            [&](Track& t) {
            t.Clear(t0, t1);
        }
            );
    };
    EditByLabel(project, tracks, selectedRegion, editfunc);

    selectedRegion.collapseToT0();

    ProjectHistory::Get(project).PushState(
        /* i18n-hint: (verb) past tense.  Audacity has just cut the labeled audio
           regions.*/
        XO("Cut labeled audio regions to clipboard"),
        /* i18n-hint: (verb)*/
        XO("Cut Labeled Audio"));
}

void OnDeleteLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto editfunc = [&](Track& track, double t0, double t1) {
        track.TypeSwitch([&](Track& t) { t.Clear(t0, t1); });
    };
    EditByLabel(project, tracks, selectedRegion, editfunc);

    selectedRegion.collapseToT0();

    ProjectHistory::Get(project).PushState(
        /* i18n-hint: (verb) Audacity has just deleted the labeled audio regions*/
        XO("Deleted labeled audio regions"),
        /* i18n-hint: (verb)*/
        XO("Delete Labeled Audio"));
}

void OnSplitCutLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto copyfunc = [&](Track& track, double t0, double t1) {
        Track::Holder result;
        track.TypeSwitch(
            [&](WaveTrack& wt) {
            result = wt.SplitCut(t0, t1);
        },
            [&](Track& t) {
            result = t.Copy(t0, t1);
            t.Silence(t0, t1);
        }
            );
        return result;
    };
    EditClipboardByLabel(project, tracks, selectedRegion, copyfunc);

    ProjectHistory::Get(project).PushState(
        XO("Cut labeled audio regions to clipboard and leave gap"),
        XO("Cut labeled audio and leave gap"));
}

void OnSplitDeleteLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto editfunc = [&](Track& track, double t0, double t1) {
        track.TypeSwitch(
            [&](WaveTrack& t) {
            t.SplitDelete(t0, t1);
        },
            [&](Track& t) {
            t.Silence(t0, t1);
        }
            );
    };
    EditByLabel(project, tracks, selectedRegion, editfunc);

    ProjectHistory::Get(project).PushState(
        /* i18n-hint: (verb) Audacity has just done a special kind of DELETE on
           the labeled audio regions */
        XO("Split Deleted labeled audio regions"),
        /* i18n-hint: (verb) Do a special kind of DELETE on labeled audio
           regions */
        XO("Split Delete Labeled Audio"));
}

void OnSilenceLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto editfunc = [&](Track& track, double t0, double t1) {
        // TODO use progress-bar utilities pending in
        // https://github.com/audacity/audacity/pull/5043
        track.TypeSwitch([&](WaveTrack& t) { t.Silence(t0, t1, {}); });
    };
    EditByLabel(project, tracks, selectedRegion, editfunc);

    ProjectHistory::Get(project).PushState(
        /* i18n-hint: (verb)*/
        XO("Silenced labeled audio regions"),
        /* i18n-hint: (verb)*/
        XO("Silence Labeled Audio"));
}

void OnCopyLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto copyfunc = [&](Track& track, double t0, double t1) {
        Track::Holder result;
        track.TypeSwitch([&](WaveTrack& wt) { result = wt.Copy(t0, t1); });
        return result;
    };
    EditClipboardByLabel(project, tracks, selectedRegion, copyfunc);

    ProjectHistory::Get(project)
    .PushState(XO("Copied labeled audio regions to clipboard"),
               /* i18n-hint: (verb)*/
               XO("Copy Labeled Audio"));
}

void OnSplitLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto editfunc = [&](Track& track, double t0, double t1) {
        track.TypeSwitch([&](WaveTrack& t) { t.Split(t0, t1); });
    };
    EditByLabel(project, tracks, selectedRegion, editfunc);

    ProjectHistory::Get(project).PushState(
        /* i18n-hint: (verb) past tense.  Audacity has just split the labeled
           audio (a point or a region)*/
        XO("Split labeled audio (points or regions)"),
        /* i18n-hint: (verb)*/
        XO("Split Labeled Audio"));
}

void OnJoinLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto editfunc = [&](Track& track, double t0, double t1, ProgressReporter reportProgress) {
        track.TypeSwitch(
            [&](WaveTrack& t) {
            // Extend Join Region by one sample, to ensure
            // that Split-Join operation on a given label is reversible and consistent
            const auto sampleTime = 1.0 / t.GetRate();
            t.Join(t0 - sampleTime, t1 + sampleTime, std::move(reportProgress));
        });
    };
    TimeStretching::WithClipRenderingProgress(
        [&](ProgressReporter progress) {
        EditByLabel(project, tracks, selectedRegion, editfunc, progress);
    });

    ProjectHistory::Get(project).PushState(
        /* i18n-hint: (verb) Audacity has just joined the labeled audio (points or
           regions) */
        XO("Joined labeled audio (points or regions)"),
        /* i18n-hint: (verb) */
        XO("Join Labeled Audio"));
}

void OnDisjoinLabels(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    if (selectedRegion.isPoint()) {
        return;
    }

    auto editfunc = [&](Track& track, double t0, double t1) {
        track.TypeSwitch([&](WaveTrack& t) {
            wxBusyCursor busy;
            t.Disjoin(t0, t1);
        });
    };
    EditByLabel(project, tracks, selectedRegion, editfunc);

    ProjectHistory::Get(project).PushState(
        /* i18n-hint: (verb) Audacity has just detached the labeled audio regions.
        This message appears in history and tells you about something
        Audacity has done.*/
        XO("Detached labeled audio regions"),
        /* i18n-hint: (verb)*/
        XO("Detach Labeled Audio"));
}

void OnNewLabelTrack(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);

    auto track = LabelTrack::Create(tracks);

    SelectUtilities::SelectNone(project);

    track->SetSelected(true);

    ProjectHistory::Get(project)
    .PushState(XO("Created new label track"), XO("New Track"));

    TrackFocus::Get(project).Set(track);
    Viewport::Get(project).ShowTrack(*track);
}

// Menu definitions

using namespace MenuRegistry;
auto LabelEditMenus()
{
    using namespace MenuRegistry;

    static const auto NotBusyLabelsAndWaveFlags
        =AudioIONotBusyFlag()
          | LabelsSelectedFlag() | WaveTracksExistFlag() | TimeSelectedFlag();

    // Returns TWO menus.

    static auto menus = std::shared_ptr{
        Items(wxT("LabelEditMenus"),

              Menu(wxT("Labels"), XXO("&Labels"),
                   Section("",
                           Command(wxT("EditLabels"), XXO("Label &Editor"), OnEditLabels,
                                   AudioIONotBusyFlag())
                           ),

                   Section("",
                           Command(wxT("AddLabel"), XXO("Add Label at &Selection"),
                                   OnAddLabel, AlwaysEnabledFlag, wxT("Ctrl+B")),
                           Command(wxT("AddLabelPlaying"),
                                   XXO("Add Label at &Playback Position"),
                                   OnAddLabelPlaying, AudioIOBusyFlag(),
   #ifdef __WXMAC__
                                   wxT("Ctrl+.")
   #else
                                   wxT("Ctrl+M")
   #endif
                                   ),
                           Command(wxT("PasteNewLabel"), XXO("Paste Te&xt to New Label"),
                                   OnPasteNewLabel,
                                   AudioIONotBusyFlag(), wxT("Ctrl+Alt+V"))
                           ),

                   Section("",
                           Command(wxT("TypeToCreateLabel"),
                                   XXO("&Typing Creates New Labels"),
                                   OnToggleTypeToCreateLabel, AlwaysEnabledFlag,
                                   Options {}.CheckTest(wxT("/GUI/TypeToCreateLabel"), false))
                           )
                   ), // first menu

              /////////////////////////////////////////////////////////////////////////////

              Menu(wxT("Labeled"), XXO("La&beled Audio"),
                   Section("",
                           /* i18n-hint: (verb)*/
                           Command(wxT("CutLabels"), XXO("&Cut"), OnCutLabels,
                                   AudioIONotBusyFlag() | LabelsSelectedFlag() | WaveTracksExistFlag()
                                   | TimeSelectedFlag(),
                                   Options { wxT("Alt+X"), XO("Cut by label") }),
                           Command(wxT("DeleteLabels"), XXO("&Delete"), OnDeleteLabels,
                                   AudioIONotBusyFlag() | LabelsSelectedFlag() | WaveTracksExistFlag()
                                   | TimeSelectedFlag(),
                                   Options { wxT("Alt+K"), XO("Delete by label") })
                           ),

                   Section("",
                           /* i18n-hint: (verb) A special way to cut out a piece of audio*/
                           Command(wxT("SplitCutLabels"), XXO("C&ut and leave gap"),
                                   OnSplitCutLabels, NotBusyLabelsAndWaveFlags,
                                   Options { wxT("Alt+Shift+X"), XO("Cut by labels and leave gap") }),
                           Command(wxT("SplitDeleteLabels"), XXO("De&lete and leave gap"),
                                   OnSplitDeleteLabels, NotBusyLabelsAndWaveFlags,
                                   Options { wxT("Alt+Shift+K"), XO("Delete by labels and leave gap") })
                           ),

                   Section("",
                           Command(wxT("SilenceLabels"), XXO("Silence &Audio"),
                                   OnSilenceLabels, NotBusyLabelsAndWaveFlags,
                                   Options { wxT("Alt+L"), XO("Label Silence") }),
                           /* i18n-hint: (verb)*/
                           Command(wxT("CopyLabels"), XXO("Co&py"), OnCopyLabels,
                                   NotBusyLabelsAndWaveFlags,
                                   Options { wxT("Alt+Shift+C"), XO("Label Copy") })
                           ),

                   Section("",
                           /* i18n-hint: (verb)*/
                           Command(wxT("SplitLabels"), XXO("Spli&t"), OnSplitLabels,
                                   AudioIONotBusyFlag() | LabelsSelectedFlag() | WaveTracksExistFlag(),
                                   Options { wxT("Alt+I"), XO("Label Split") }),
                           /* i18n-hint: (verb)*/
                           Command(wxT("JoinLabels"), XXO("&Join"), OnJoinLabels,
                                   NotBusyLabelsAndWaveFlags,
                                   Options { wxT("Alt+J"), XO("Label Join") }),
                           Command(wxT("DisjoinLabels"), XXO("Detac&h at Silences"),
                                   OnDisjoinLabels, NotBusyLabelsAndWaveFlags,
                                   wxT("Alt+Shift+J"))
                           )
                   ) // second menu

              ) }; // two menus
    return menus;
}

AttachedItem sAttachment1{ Indirect(LabelEditMenus()),
                           { wxT("Edit/Other"),
                             { OrderingHint::Before, wxT("EditMetaData") } }
};

AttachedItem sAttachment2{
    Command(wxT("NewLabelTrack"), XXO("&Label Track"),
            OnNewLabelTrack, AudioIONotBusyFlag()),
    wxT("Tracks/Add/Add")
};
}
