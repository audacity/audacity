/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelResizeHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TrackPanelResizeHandle.h"

#include <wx/cursor.h>
#include <wx/translation.h>

#include "HitTestResult.h"
#include "ProjectHistory.h"
#include "RefreshCode.h"
#include "Track.h"
#include "TrackPanelMouseEvent.h"
#include "tracks/ui/TrackView.h"

HitTestPreview TrackPanelResizeHandle::HitPreview(bool bLinked)
{
   // TODO: more-than-two-channels-message

   static wxCursor resizeCursor{ wxCURSOR_SIZENS };

   /// When in the resize area we can adjust size or relative size.
   // Check to see whether it is the first channel of a stereo track
   if (bLinked) {
      // If we are in the label we got here 'by mistake' and we're
      // not actually in the resize area at all.  (The resize area
      // is shorter when it is between stereo tracks).

      return {
         XO(
"Click and drag to adjust relative size of stereo tracks, double-click to make heights equal"),
         &resizeCursor
      };
   }
   else {
      return {
         XO("Click and drag to resize the track."),
         &resizeCursor
      };
   }
}

TrackPanelResizeHandle::~TrackPanelResizeHandle()
{
}

UIHandle::Result TrackPanelResizeHandle::Click(
   const TrackPanelMouseEvent &evt, AudacityProject *pProject )
{
   using namespace RefreshCode;
   if ( evt.event.LeftDClick() && mMode == IsResizingBetweenLinkedTracks ) {
      auto &tracks = TrackList::Get( *pProject );
      auto pTrack = tracks.Lock(mpTrack);
      if (pTrack &&
          !TrackView::Get(*pTrack).GetMinimized()) {
         auto range = TrackList::Channels( pTrack.get() );
         auto size = range.size();
         auto height = range.sum( [](const Track *pTrack){
            return TrackView::Get(*pTrack).GetHeight(); } );
         int ii = 1;
         int coord = 0;
         for ( const auto channel : range ) {
            int newCoord = ((double)ii++ /size) * height;
            TrackView::Get(*channel).SetExpandedHeight( newCoord - coord );
            coord = newCoord;
         }
         ProjectHistory::Get( *pProject ).ModifyState(false);
         // Do not start a drag
         return Cancelled | RefreshAll;
      }
   }
   return RefreshNone;
}

TrackPanelResizeHandle::TrackPanelResizeHandle
( const std::shared_ptr<Track> &track, int y )
   : mpTrack{ track }
   , mMouseClickY( y )
{
   // TODO: more-than-two-channels

   //STM:  Determine whether we should rescale one or two tracks
   auto channels = TrackList::Channels(track.get());
   auto last = *channels.rbegin();
   auto &lastView = TrackView::Get( *last );
   mInitialTrackHeight = lastView.GetHeight();
   mInitialExpandedHeight = lastView.GetExpandedHeight();
   mInitialMinimized = lastView.GetMinimized();

   if (channels.size() > 1) {
      auto first = *channels.begin();
      auto &firstView = TrackView::Get( *first );

      mInitialUpperTrackHeight = firstView.GetHeight();
      mInitialUpperExpandedHeight = firstView.GetExpandedHeight();

      if (track.get() == *channels.rbegin())
         // capturedTrack is the lowest track
         mMode = IsResizingBelowLinkedTracks;
      else
         // capturedTrack is not the lowest track
         mMode = IsResizingBetweenLinkedTracks;
   }
   else
      mMode = IsResizing;
}

UIHandle::Result TrackPanelResizeHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto &tracks = TrackList::Get( *pProject );
   auto pTrack = tracks.Lock(mpTrack);
   if ( !pTrack )
      return RefreshCode::Cancelled;

   auto &view = TrackView::Get( *pTrack );

   const wxMouseEvent &event = evt.event;

   int delta = (event.m_y - mMouseClickY);

   // On first drag, jump out of minimized mode.  Initial height
   // will be height of minimized track.
   //
   // This used to be in HandleResizeClick(), but simply clicking
   // on a resize border would switch the minimized state.
   auto &data = TrackView::Get( *pTrack );
   if (data.GetMinimized()) {
      auto channels = TrackList::Channels( pTrack.get() );
      for (auto channel : channels) {
         auto &channelView = TrackView::Get( *channel );
         channelView.SetExpandedHeight(channelView.GetHeight());
         channelView.SetMinimized( false );
      }

      if (channels.size() > 1) {
         // Initial values must be reset since they weren't based on the
         // minimized heights.
         auto &channelView = TrackView::Get( **channels.begin() );
         mInitialUpperTrackHeight = channelView.GetHeight();
         mInitialTrackHeight = channelView.GetHeight();
      }
   }

   // Common pieces of code for MONO_WAVE_PAN and otherwise.
   auto doResizeBelow = [&] (Track *prev, bool WXUNUSED(vStereo)) {
      // TODO: more-than-two-channels
      
      auto &prevView = TrackView::Get( *prev );

      double proportion = static_cast < double >(mInitialTrackHeight)
      / (mInitialTrackHeight + mInitialUpperTrackHeight);

      int newTrackHeight = static_cast < int >
      (mInitialTrackHeight + delta * proportion);

      int newUpperTrackHeight = static_cast < int >
      (mInitialUpperTrackHeight + delta * (1.0 - proportion));

      //make sure neither track is smaller than its minimum height
      if (newTrackHeight < view.GetMinimizedHeight())
         newTrackHeight = view.GetMinimizedHeight();
      if (newUpperTrackHeight < prevView.GetMinimizedHeight())
         newUpperTrackHeight = prevView.GetMinimizedHeight();

      view.SetExpandedHeight(newTrackHeight);
      prevView.SetExpandedHeight(newUpperTrackHeight);
   };

   auto doResizeBetween = [&] (Track *next, bool WXUNUSED(vStereo)) {
      // TODO: more-than-two-channels

      auto &nextView = TrackView::Get( *next );
      int newUpperTrackHeight = mInitialUpperTrackHeight + delta;
      int newTrackHeight = mInitialTrackHeight - delta;

      // make sure neither track is smaller than its minimum height
      if (newTrackHeight < nextView.GetMinimizedHeight()) {
         newTrackHeight = nextView.GetMinimizedHeight();
         newUpperTrackHeight =
         mInitialUpperTrackHeight + mInitialTrackHeight - nextView.GetMinimizedHeight();
      }
      if (newUpperTrackHeight < view.GetMinimizedHeight()) {
         newUpperTrackHeight = view.GetMinimizedHeight();
         newTrackHeight =
         mInitialUpperTrackHeight + mInitialTrackHeight - view.GetMinimizedHeight();
      }

      view.SetExpandedHeight(newUpperTrackHeight);
      nextView.SetExpandedHeight(newTrackHeight);
   };

   auto doResize = [&] {
      int newTrackHeight = mInitialTrackHeight + delta;
      if (newTrackHeight < view.GetMinimizedHeight())
         newTrackHeight = view.GetMinimizedHeight();
      view.SetExpandedHeight(newTrackHeight);
   };

   //STM: We may be dragging one or two (stereo) tracks.
   // If two, resize proportionally if we are dragging the lower track, and
   // adjust compensatively if we are dragging the upper track.

   switch( mMode )
   {
      case IsResizingBelowLinkedTracks:
      {
         auto prev = * -- tracks.Find(pTrack.get());
         doResizeBelow(prev, false);
         break;
      }
      case IsResizingBetweenLinkedTracks:
      {
         auto next = * ++ tracks.Find(pTrack.get());
         doResizeBetween(next, false);
         break;
      }
      case IsResizing:
      {
         doResize();
         break;
      }
      default:
         // don't refresh in this case.
         return RefreshCode::RefreshNone;
   }

   return RefreshCode::RefreshAll;
}

HitTestPreview TrackPanelResizeHandle::Preview
(const TrackPanelMouseState &, AudacityProject *)
{
   return HitPreview(mMode == IsResizingBetweenLinkedTracks);
}

UIHandle::Result TrackPanelResizeHandle::Release
(const TrackPanelMouseEvent &, AudacityProject *pProject,
 wxWindow *)
{
   ///  This happens when the button is released from a drag.
   ///  Since we actually took care of resizing the track when
   ///  we got drag events, all we have to do here is clean up.
   ///  We also modify the undo state (the action doesn't become
   ///  undo-able, but it gets merged with the previous undo-able
   ///  event).
   ProjectHistory::Get( *pProject ).ModifyState(false);
   return RefreshCode::FixScrollbars;
}

UIHandle::Result TrackPanelResizeHandle::Cancel(AudacityProject *pProject)
{
   auto &tracks = TrackList::Get( *pProject );
   auto pTrack = tracks.Lock(mpTrack);
   if ( !pTrack )
      return RefreshCode::Cancelled;


   switch (mMode) {
   case IsResizing:
   {
      auto &view = TrackView::Get( *pTrack );
      view.SetExpandedHeight(mInitialExpandedHeight);
      view.SetMinimized( mInitialMinimized );
   }
   break;
   case IsResizingBetweenLinkedTracks:
   {
      Track *const next = * ++ tracks.Find(pTrack.get());
      auto
         &view = TrackView::Get( *pTrack ), &nextView = TrackView::Get( *next );
      view.SetExpandedHeight(mInitialUpperExpandedHeight);
      view.SetMinimized( mInitialMinimized );
      nextView.SetExpandedHeight(mInitialExpandedHeight);
      nextView.SetMinimized( mInitialMinimized );
   }
   break;
   case IsResizingBelowLinkedTracks:
   {
      Track *const prev = * -- tracks.Find(pTrack.get());
      auto
         &view = TrackView::Get( *pTrack ), &prevView = TrackView::Get( *prev );
      view.SetExpandedHeight(mInitialExpandedHeight);
      view.SetMinimized( mInitialMinimized );
      prevView.SetExpandedHeight(mInitialUpperExpandedHeight);
      prevView.SetMinimized(mInitialMinimized);
   }
   break;
   }

   return RefreshCode::RefreshAll;
}
