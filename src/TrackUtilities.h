/**********************************************************************
 
 Sneedacity: A Digital Audio Editor
 
 TrackUtilities.h
 
 Paul Licameli split from TrackMenus.h
 
 **********************************************************************/

#ifndef __SNEEDACITY_TRACK_UTILITIES__
#define __SNEEDACITY_TRACK_UTILITIES__

class SneedacityProject;
class Track;

namespace TrackUtilities {

   enum MoveChoice {
      OnMoveUpID, OnMoveDownID, OnMoveTopID, OnMoveBottomID
   };
   /// Move a track up, down, to top or to bottom.
   SNEEDACITY_DLL_API void DoMoveTrack(
      SneedacityProject &project, Track* target, MoveChoice choice );
   // "exclusive" mute means mute the chosen track and unmute all others.
   SNEEDACITY_DLL_API
   void DoTrackMute( SneedacityProject &project, Track *pTrack, bool exclusive );
   // Type of solo (standard or simple) follows the set preference, unless
   // exclusive == true, which causes the opposite behavior.
   SNEEDACITY_DLL_API
   void DoTrackSolo( SneedacityProject &project, Track *pTrack, bool exclusive );
   SNEEDACITY_DLL_API
   void DoRemoveTrack( SneedacityProject &project, Track * toRemove );
   SNEEDACITY_DLL_API
   void DoRemoveTracks( SneedacityProject & );

}

#endif
