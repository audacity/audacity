/**********************************************************************

 Audacity: A Digital Audio Editor

 SelectionState.h

 **********************************************************************/

#ifndef __AUDACITY_SELECTION_STATE__
#define __AUDACITY_SELECTION_STATE__

class Track;
class TrackList;
class MixerBoard;
class ViewInfo;

// State relating to the set of selected tracks
class SelectionState
{
public:
   static void SelectTrackLength
      ( TrackList &tracks, ViewInfo &viewInfo, Track &track, bool syncLocked );

   void SelectTrack
      ( TrackList &tracks, Track &track,
        bool selected, bool updateLastPicked, MixerBoard *pMixerBoard );
   // Inclusive range of tracks, the limits specified in either order:
   void SelectRangeOfTracks
      ( TrackList &tracks, Track &sTrack, Track &eTrack,
        MixerBoard *pMixerBoard );
   void SelectNone( TrackList &tracks, MixerBoard *pMixerBoard );
   void ChangeSelectionOnShiftClick
      ( TrackList &tracks, Track &track, MixerBoard *pMixerBoard );
   void HandleListSelection
      ( TrackList &tracks, ViewInfo &viewInfo, Track &track,
        bool shift, bool ctrl, bool syncLocked, MixerBoard *pMixerBoard );

   void TrackListUpdated( const TrackList &tracks );

   Track *mLastPickedTrack {};
};

#endif
