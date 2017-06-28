/**********************************************************************

 Audacity: A Digital Audio Editor

 SelectionState.h

 **********************************************************************/

#include "Audacity.h"
#include "SelectionState.h"
#include "MixerBoard.h"
#include "ViewInfo.h"
#include "Track.h"

// Set selection length to the length of a track -- but if sync-lock is turned
// on, use the largest possible selection in the sync-lock group.
// If it's a stereo track, do the same for the stereo channels.
void SelectionState::SelectTrackLength
( TrackList &tracks, ViewInfo &viewInfo, Track &track, bool syncLocked )
{
   SyncLockedTracksIterator it( &tracks );
   Track *t1 = it.StartWith( &track );
   double minOffset = track.GetOffset();
   double maxEnd = track.GetEndTime();

   // If we have a sync-lock group and sync-lock linking is on,
   // check the sync-lock group tracks.
   if ( syncLocked && t1 != NULL )
   {
      for ( ; t1; t1 = it.Next())
      {
         if (t1->GetOffset() < minOffset)
            minOffset = t1->GetOffset();
         if (t1->GetEndTime() > maxEnd)
            maxEnd = t1->GetEndTime();
      }
   }
   else
   {
      // Otherwise, check for a stereo pair
      t1 = track.GetLink();
      if (t1)
      {
         if (t1->GetOffset() < minOffset)
            minOffset = t1->GetOffset();
         if (t1->GetEndTime() > maxEnd)
            maxEnd = t1->GetEndTime();
      }
   }

   // PRL: double click or click on track control.
   // should this select all frequencies too?  I think not.
   viewInfo.selectedRegion.setTimes(minOffset, maxEnd);
}

void SelectionState::SelectTrack
( TrackList &tracks, Track &track, bool selected, bool updateLastPicked,
  MixerBoard *pMixerBoard )
{
   bool wasCorrect = (selected == track.GetSelected());

   tracks.Select( &track, selected );
   if (updateLastPicked)
      mLastPickedTrack = Track::Pointer( &track );

//The older code below avoids an anchor on an unselected track.

   /*
   if (selected) {
      // This handles the case of linked tracks, selecting all channels
      mTracks->Select(pTrack, true);
      if (updateLastPicked)
         mLastPickedTrack = Track::Pointer( pTrack );
   }
   else {
      mTracks->Select(pTrack, false);
      if (updateLastPicked && pTrack == mLastPickedTrack.lock().get())
         mLastPickedTrack.reset();
   }
*/

   // Update mixer board, but only as needed so it does not flicker.
   if (!wasCorrect) {
      auto pt = dynamic_cast< PlayableTrack* >( &track );
      if (pMixerBoard && pt)
         pMixerBoard->RefreshTrackCluster( pt );
   }
}

void SelectionState::SelectRangeOfTracks
( TrackList &tracks, Track &rsTrack, Track &reTrack, MixerBoard *pMixerBoard )
{
   Track *sTrack = &rsTrack, *eTrack = &reTrack;
   // Swap the track pointers if needed
   if (eTrack->GetIndex() < sTrack->GetIndex())
      std::swap(sTrack, eTrack);

   TrackListIterator iter( &tracks );
   sTrack = iter.StartWith( sTrack );
   do {
      SelectTrack( tracks, *sTrack, true, false, pMixerBoard );
      if ( sTrack == eTrack ) {
         break;
      }

      sTrack = iter.Next();
   } while ( sTrack );
}

void SelectionState::SelectNone( TrackList &tracks, MixerBoard *pMixerBoard )
{
   TrackListIterator iter( &tracks );
   Track *track = iter.First();
   while ( track ) {
      SelectTrack( tracks, *track, false, false, pMixerBoard );
      track = iter.Next();
   }
}

void SelectionState::ChangeSelectionOnShiftClick
( TrackList &tracks, Track &track, MixerBoard *pMixerBoard )
{

   // Optional: Track already selected?  Nothing to do.
   // If we enable this, Shift-Click behaves like click in this case.
   //if( pTrack->GetSelected() )
   //   return;

   // Find first and last selected track.
   Track* pFirst = nullptr;
   Track* pLast = nullptr;
   // We will either extend from the first or from the last.
   auto pExtendFrom = tracks.Lock(mLastPickedTrack);

   if( !pExtendFrom ) {
      TrackListIterator iter( &tracks );
      for (Track *t = iter.First(); t; t = iter.Next()) {
         const bool isSelected = t->GetSelected();
         // Record first and last selected.
         if( isSelected ) {
            if( !pFirst )
               pFirst = t;
            pLast = t;
         }
         // If our track is at or after the first, extend from the first.
         if( t == &track )
            pExtendFrom = Track::Pointer( pFirst );
      }
      // Our track was earlier than the first.  Extend from the last.
      if( !pExtendFrom )
         pExtendFrom = Track::Pointer( pLast );
   }

   SelectNone( tracks, pMixerBoard );
   if( pExtendFrom )
      SelectRangeOfTracks( tracks, track, *pExtendFrom, pMixerBoard );
   else
      SelectTrack( tracks, track, true, true, pMixerBoard );
   mLastPickedTrack = pExtendFrom;
}

void SelectionState::HandleListSelection
( TrackList &tracks, ViewInfo &viewInfo,
  Track &track, bool shift, bool ctrl, bool syncLocked, MixerBoard *pMixerBoard )
{
   // AS: If the shift button is being held down, invert
   //  the selection on this track.
   if (ctrl)
      SelectTrack( tracks, track, !track.GetSelected(), true, pMixerBoard );
   else {
      if (shift && mLastPickedTrack.lock())
         ChangeSelectionOnShiftClick( tracks, track, pMixerBoard );
      else {
         SelectNone( tracks, pMixerBoard );
         SelectTrack( tracks, track, true, true, pMixerBoard );
         SelectTrackLength( tracks, viewInfo, track, syncLocked );
      }

      if (pMixerBoard)
         pMixerBoard->RefreshTrackClusters();
   }
}

SelectionStateChanger::SelectionStateChanger
( SelectionState &state, TrackList &tracks )
   : mpState{ &state }
   , mTracks{ tracks }
   , mInitialLastPickedTrack{ state.mLastPickedTrack }
{
   // Save initial state of track selections
   mInitialTrackSelection.clear();
   TrackListIterator iter( &mTracks );
   for (Track *track = iter.First(); track; track = iter.Next()) {
      const bool isSelected = track->GetSelected();
      mInitialTrackSelection.push_back(isSelected);
   }
}

SelectionStateChanger::~SelectionStateChanger()
{
   if ( mpState ) {
      // roll back changes
      mpState->mLastPickedTrack = mInitialLastPickedTrack;
      TrackListIterator iter( &mTracks );
      std::vector<bool>::const_iterator
         it = mInitialTrackSelection.begin(),
         end = mInitialTrackSelection.end();
      for (Track *track = iter.First(); track && it != end; track = iter.Next()) {
         // wxASSERT(it != end);
         track->SetSelected( *it++ );
      }
   }
}

void SelectionStateChanger::Commit()
{
   mpState = nullptr;
}
