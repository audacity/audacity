/**********************************************************************

 Audacity: A Digital Audio Editor

 SelectionState.h

 **********************************************************************/

#ifndef __AUDACITY_SELECTION_STATE__
#define __AUDACITY_SELECTION_STATE__

class AudacityProject;
class Track;
class TrackList;
class ViewInfo;
#include "ClientData.h"
#include <memory>
#include <vector>

// State relating to the set of selected tracks
class TRACK_SELECTION_API SelectionState final : public ClientData::Base
{
public:
    SelectionState() = default;
    SelectionState(const SelectionState&) = delete;
    SelectionState& operator=(const SelectionState&) = delete;

    static SelectionState& Get(AudacityProject& project);
    static const SelectionState& Get(const AudacityProject& project);

    /*!
     Set selection length to the length of a track -- but if sync-lock is turned
     on, use the largest possible selection in the sync-lock group.
     If it's a stereo track, do the same for the stereo channels.
     */
    static void SelectTrackLength(
        ViewInfo& viewInfo, Track& track, bool syncLocked);

    void SelectTrack(
        Track& track, bool selected, bool updateLastPicked);
    // Inclusive range of tracks, the limits specified in either order:
    void SelectRangeOfTracks(TrackList& tracks, Track& sTrack, Track& eTrack);
    void SelectNone(TrackList& tracks);
    void ChangeSelectionOnShiftClick(TrackList& tracks, Track& track);
    void HandleListSelection(TrackList& tracks, ViewInfo& viewInfo, Track& track, bool shift, bool ctrl, bool syncLocked);

private:
    friend class SelectionStateChanger;
    std::weak_ptr<Track> mLastPickedTrack;
};

// For committing or rolling-back of changes in selectedness of tracks.
// When rolling back, it is assumed that no tracks have been added or removed.
class TRACK_SELECTION_API SelectionStateChanger
{
public:
    SelectionStateChanger(SelectionState& state, TrackList& tracks);
    SelectionStateChanger(const SelectionStateChanger&) = delete;
    SelectionStateChanger& operator=(const SelectionStateChanger&) = delete;

    ~SelectionStateChanger();
    void Commit();

private:
    SelectionState* mpState;
    TrackList& mTracks;
    std::weak_ptr<Track> mInitialLastPickedTrack;
    std::vector<bool> mInitialTrackSelection;
};

#endif
