/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackUtilities.cpp

 Paul Licameli split from TrackMenus.cpp

 **********************************************************************/

#include "TrackUtilities.h"

#include "PlayableTrack.h"
#include "ProjectHistory.h"
#include "TrackFocus.h"
#include "TrackPanel.h"
#include "Viewport.h"

void TrackUtilities::DoRemoveTracks(AudacityProject& project)
{
    auto& tracks = TrackList::Get(project);
    auto& trackPanel = TrackPanel::Get(project);

    auto range = tracks.Selected();
    using Iter = decltype(range.begin());

    // Find the track preceding the first removed track
    std::optional<Iter> focus;
    if (!range.empty()) {
        auto iter = tracks.Find(*range.begin());
        // TrackIter allows decrement even of begin iterators
        focus.emplace(--iter);
    }

    while (!range.empty()) {
        tracks.Remove(**range.first++);
    }

    if (!(focus.has_value() && **focus)) {
        // try to use the last track
        focus.emplace(tracks.end().advance(-1));
    }
    assert(focus);
    Track* f = **focus;
    // Try to use the first track after the removal
    // TrackIter allows increment even of end iterators
    if (const auto nextF = *++*focus) {
        f = nextF;
    }

    // If we actually have something left, then set focus and make sure it's seen
    if (f) {
        TrackFocus::Get(project).Set(f);
        Viewport::Get(project).ShowTrack(*f);
    }

    ProjectHistory::Get(project)
    .PushState(XO("Removed audio track(s)"), XO("Remove Track"));

    trackPanel.UpdateViewIfNoTracks();
}

void TrackUtilities::DoTrackMute(
    AudacityProject& project, Track& track, bool exclusive)
{
    auto& tracks = TrackList::Get(project);

    // "exclusive" mute means mute the chosen track and unmute all others.
    if (exclusive) {
        for (auto playable : tracks.Any<PlayableTrack>()) {
            bool chosen = (&track == playable);
            playable->SetMute(chosen);
            playable->SetSolo(false);
        }
    } else {
        // Normal click toggles this track.
        auto pt = dynamic_cast<PlayableTrack*>(&track);
        if (!pt) {
            return;
        }

        bool wasMute = pt->GetMute();
        pt->SetMute(!wasMute);

        if (auto value = TracksBehaviorsSolo.ReadEnum();
            value == SoloBehaviorSimple) {
            // We also set a solo indicator if we have just one track / stereo pair playing.
            // in a group of more than one playable tracks.
            // otherwise clear solo on everything.

            auto range = tracks.Any<PlayableTrack>();
            auto nPlayableTracks = range.size();
            auto nPlaying = (range - &PlayableTrack::GetMute).size();
            for (auto track : range) {
                track->SetSolo((nPlaying == 1)
                               && (nPlayableTracks > 1) && !track->GetMute());
            }
        }
    }
    ProjectHistory::Get(project).ModifyState(true);

    TrackFocus::Get(project).UpdateAccessibility();
}

void TrackUtilities::DoTrackSolo(
    AudacityProject& project, Track& track, bool exclusive)
{
    auto& tracks = TrackList::Get(project);

    const auto pt = dynamic_cast<PlayableTrack*>(&track);
    if (!pt) {
        return;
    }
    bool bWasSolo = pt->GetSolo();

    bool simple = (TracksBehaviorsSolo.ReadEnum() == SoloBehaviorSimple);
    bool bSoloMultiple = !simple ^ exclusive;

    // Standard and Simple solo have opposite defaults:
    //   Standard - Behaves as individual buttons, shift=radio buttons
    //   Simple   - Behaves as radio buttons, shift=individual
    // In addition, Simple solo will mute/unmute tracks
    // when in standard radio button mode.
    if (bSoloMultiple) {
        pt->SetSolo(!bWasSolo);
    } else {
        // Normal click solo this track only, mute everything else.
        // OR unmute and unsolo everything.
        for (auto playable : tracks.Any<PlayableTrack>()) {
            bool chosen = (&track == playable);
            if (chosen) {
                playable->SetSolo(!bWasSolo);
                if (simple) {
                    playable->SetMute(false);
                }
            } else {
                playable->SetSolo(false);
                if (simple) {
                    playable->SetMute(!bWasSolo);
                }
            }
        }
    }
    ProjectHistory::Get(project).ModifyState(true);

    TrackFocus::Get(project).UpdateAccessibility();
}

void TrackUtilities::DoRemoveTrack(AudacityProject& project, Track& toRemove)
{
    auto& tracks = TrackList::Get(project);
    auto& trackFocus = TrackFocus::Get(project);

    const auto iter = tracks.Find(&toRemove);

    // If it was focused, then NEW focus is the next or, if
    // unavailable, the previous track. (The NEW focus is set
    // after the track has been removed.)
    bool toRemoveWasFocused = trackFocus.Get() == *iter;
    std::optional<decltype(iter)> newFocus{};
    if (toRemoveWasFocused) {
        auto iterNext = iter,
             iterPrev = iter;
        newFocus.emplace(++iterNext);
        if (!**newFocus) {
            newFocus.emplace(--iterPrev);
        }
    }

    wxString name = toRemove.GetName();

    tracks.Remove(**iter);

    if (toRemoveWasFocused) {
        trackFocus.Set(**newFocus);
    }

    ProjectHistory::Get(project).PushState(
        XO("Removed track '%s'.").Format(name),
        XO("Track Remove"));
}

void TrackUtilities::DoMoveTrack(
    AudacityProject& project, Track& target, MoveChoice choice)
{
    auto& tracks = TrackList::Get(project);

    TranslatableString longDesc, shortDesc;

    switch (choice) {
    case OnMoveTopID:
        /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
        longDesc = XO("Moved '%s' to Top");
        shortDesc = XO("Move Track to Top");

        // TODO: write TrackList::Rotate to do this in one step and avoid emitting
        // an event for each swap
        while (tracks.CanMoveUp(target)) {
            tracks.Move(target, true);
        }

        break;
    case OnMoveBottomID:
        /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
        longDesc = XO("Moved '%s' to Bottom");
        shortDesc = XO("Move Track to Bottom");

        // TODO: write TrackList::Rotate to do this in one step and avoid emitting
        // an event for each swap
        while (tracks.CanMoveDown(target)) {
            tracks.Move(target, false);
        }

        break;
    default:
        bool bUp = (OnMoveUpID == choice);

        tracks.Move(target, bUp);
        longDesc
            =/* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
              bUp ? XO("Moved '%s' Up")
              : XO("Moved '%s' Down");
        shortDesc
            =/* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
              bUp ? XO("Move Track Up")
              : XO("Move Track Down");
    }

    longDesc.Format(target.GetName());

    ProjectHistory::Get(project).PushState(longDesc, shortDesc);
}
