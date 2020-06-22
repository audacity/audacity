/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 TrackUtilities.cpp
 
 Paul Licameli split from TrackMenus.cpp
 
 **********************************************************************/

#include "TrackUtilities.h"

#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "Track.h"
#include "TrackPanelAx.h"
#include "TrackPanel.h"

namespace TrackUtilities {

void DoRemoveTracks( AudacityProject &project )
{
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );

   std::vector<Track*> toRemove;
   for (auto track : tracks.Selected())
      toRemove.push_back(track);

   // Capture the track preceding the first removed track
   Track *f{};
   if (!toRemove.empty()) {
      auto found = tracks.Find(toRemove[0]);
      f = *--found;
   }

   for (auto track : toRemove)
      tracks.Remove(track);

   if (!f)
      // try to use the last track
      f = *tracks.Any().rbegin();
   if (f) {
      // Try to use the first track after the removal
      auto found = tracks.FindLeader(f);
      auto t = *++found;
      if (t)
         f = t;
   }

   // If we actually have something left, then set focus and make sure it's seen
   if (f) {
      TrackFocus::Get(project).Set(f);
      f->EnsureVisible();
   }

   ProjectHistory::Get( project )
      .PushState(XO("Removed audio track(s)"), XO("Remove Track"));

   trackPanel.UpdateViewIfNoTracks();
}

void DoTrackMute(AudacityProject &project, Track *t, bool exclusive)
{
   const auto &settings = ProjectSettings::Get( project );
   auto &tracks = TrackList::Get( project );

   // Whatever t is, replace with lead channel
   t = *tracks.FindLeader(t);

   // "exclusive" mute means mute the chosen track and unmute all others.
   if (exclusive) {
      for (auto leader : tracks.Leaders<PlayableTrack>()) {
         const auto group = TrackList::Channels(leader);
         bool chosen = (t == leader);
         for (auto channel : group)
            channel->SetMute( chosen ),
            channel->SetSolo( false );
      }
   }
   else {
      // Normal click toggles this track.
      auto pt = dynamic_cast<PlayableTrack *>( t );
      if (!pt)
         return;

      bool wasMute = pt->GetMute();
      for (auto channel : TrackList::Channels(pt))
         channel->SetMute( !wasMute );

      if (settings.IsSoloSimple() || settings.IsSoloNone())
      {
         // We also set a solo indicator if we have just one track / stereo pair playing.
         // in a group of more than one playable tracks.
         // otherwise clear solo on everything.

         auto range = tracks.Leaders<PlayableTrack>();
         auto nPlayableTracks = range.size();
         auto nPlaying = (range - &PlayableTrack::GetMute).size();

         for (auto track : tracks.Any<PlayableTrack>())
            // will set both of a stereo pair
            track->SetSolo( (nPlaying==1) && (nPlayableTracks > 1 ) && !track->GetMute() );
      }
   }
   ProjectHistory::Get( project ).ModifyState(true);

   TrackFocus::Get( project ).UpdateAccessibility();
}

void DoTrackSolo(AudacityProject &project, Track *t, bool exclusive)
{
   const auto &settings = ProjectSettings::Get( project );
   auto &tracks = TrackList::Get( project );
   
   // Whatever t is, replace with lead channel
   t = *tracks.FindLeader(t);

   const auto pt = dynamic_cast<PlayableTrack *>( t );
   if (!pt)
      return;
   bool bWasSolo = pt->GetSolo();

   bool bSoloMultiple = !settings.IsSoloSimple() ^ exclusive;

   // Standard and Simple solo have opposite defaults:
   //   Standard - Behaves as individual buttons, shift=radio buttons
   //   Simple   - Behaves as radio buttons, shift=individual
   // In addition, Simple solo will mute/unmute tracks
   // when in standard radio button mode.
   if ( bSoloMultiple )
   {
      for (auto channel : TrackList::Channels(pt))
         channel->SetSolo( !bWasSolo );
   }
   else
   {
      // Normal click solo this track only, mute everything else.
      // OR unmute and unsolo everything.
      for (auto leader : tracks.Leaders<PlayableTrack>()) {
         const auto group = TrackList::Channels(leader);
         bool chosen = (t == leader);
         for (auto channel : group) {
            if (chosen) {
               channel->SetSolo( !bWasSolo );
               if( settings.IsSoloSimple() )
                  channel->SetMute( false );
            }
            else {
               channel->SetSolo( false );
               if( settings.IsSoloSimple() )
                  channel->SetMute( !bWasSolo );
            }
         }
      }
   }
   ProjectHistory::Get( project ).ModifyState(true);

   TrackFocus::Get( project ).UpdateAccessibility();
}

void DoRemoveTrack(AudacityProject &project, Track * toRemove)
{
   auto &tracks = TrackList::Get( project );
   auto &trackFocus = TrackFocus::Get( project );
   auto &window = ProjectWindow::Get( project );

   // If it was focused, then NEW focus is the next or, if
   // unavailable, the previous track. (The NEW focus is set
   // after the track has been removed.)
   bool toRemoveWasFocused = trackFocus.Get() == toRemove;
   Track* newFocus{};
   if (toRemoveWasFocused) {
      auto iterNext = tracks.FindLeader(toRemove), iterPrev = iterNext;
      newFocus = *++iterNext;
      if (!newFocus) {
         newFocus = *--iterPrev;
      }
   }

   wxString name = toRemove->GetName();

   auto channels = TrackList::Channels(toRemove);
   // Be careful to post-increment over positions that get erased!
   auto &iter = channels.first;
   while (iter != channels.end())
      tracks.Remove( * iter++ );

   if (toRemoveWasFocused)
      trackFocus.Set( newFocus );

   ProjectHistory::Get( project ).PushState(
      XO("Removed track '%s.'").Format( name ),
      XO("Track Remove"));
}

void DoMoveTrack
(AudacityProject &project, Track* target, MoveChoice choice)
{
   auto &tracks = TrackList::Get( project );

   TranslatableString longDesc, shortDesc;

   switch (choice)
   {
   case OnMoveTopID:
      /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
      longDesc = XO("Moved '%s' to Top");
      shortDesc = XO("Move Track to Top");

      // TODO: write TrackList::Rotate to do this in one step and avoid emitting
      // an event for each swap
      while (tracks.CanMoveUp(target))
         tracks.Move(target, true);

      break;
   case OnMoveBottomID:
      /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
      longDesc = XO("Moved '%s' to Bottom");
      shortDesc = XO("Move Track to Bottom");

      // TODO: write TrackList::Rotate to do this in one step and avoid emitting
      // an event for each swap
      while (tracks.CanMoveDown(target))
         tracks.Move(target, false);

      break;
   default:
      bool bUp = (OnMoveUpID == choice);

      tracks.Move(target, bUp);
      longDesc =
         /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
         bUp? XO("Moved '%s' Up")
         : XO("Moved '%s' Down");
      shortDesc =
         /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
         bUp? XO("Move Track Up")
         : XO("Move Track Down");

   }

   longDesc.Format(target->GetName());

   ProjectHistory::Get( project ).PushState(longDesc, shortDesc);
}

}
