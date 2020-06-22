/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 TrackUtilities.h
 
 Paul Licameli split from TrackMenus.h
 
 **********************************************************************/

#ifndef __AUDACITY_TRACK_UTILITIES__
#define __AUDACITY_TRACK_UTILITIES__

class AudacityProject;
class Track;

namespace TrackUtilities {

   enum MoveChoice {
      OnMoveUpID, OnMoveDownID, OnMoveTopID, OnMoveBottomID
   };
   /// Move a track up, down, to top or to bottom.
   void DoMoveTrack( AudacityProject &project, Track* target, MoveChoice choice );
   // "exclusive" mute means mute the chosen track and unmute all others.
   void DoTrackMute( AudacityProject &project, Track *pTrack, bool exclusive );
   // Type of solo (standard or simple) follows the set preference, unless
   // exclusive == true, which causes the opposite behavior.
   void DoTrackSolo( AudacityProject &project, Track *pTrack, bool exclusive );
   void DoRemoveTrack( AudacityProject &project, Track * toRemove );
   void DoRemoveTracks( AudacityProject & );

}

#endif
