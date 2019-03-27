/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackPanelCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "CommonTrackPanelCell.h"

#include "../../Experimental.h"

#include "Scrubbing.h"

#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../Track.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelMouseEvent.h"

CommonTrackPanelCell::~CommonTrackPanelCell()
{
}

HitTestPreview CommonTrackPanelCell::DefaultPreview
(const TrackPanelMouseState &, const AudacityProject *)
{
   static wxCursor defaultCursor{ wxCURSOR_ARROW };
   return { {}, &defaultCursor, {} };
}

unsigned CommonTrackPanelCell::HandleWheelRotation
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;

   if (pProject->GetTracks()->empty())
      // Scrolling and Zoom in and out commands are disabled when there are no tracks.
      // This should be disabled too for consistency.  Otherwise
      // you do see changes in the time ruler.
      return Cancelled;

   unsigned result = RefreshAll;
   const wxMouseEvent &event = evt.event;
   ViewInfo &viewInfo = pProject->GetViewInfo();
   Scrubber &scrubber = pProject->GetScrubber();
   const auto steps = evt.steps;

   if (event.ShiftDown()
       // Don't pan during smooth scrolling.  That would conflict with keeping
       // the play indicator centered.
       && !scrubber.IsScrollScrubbing()
      )
   {
      // MM: Scroll left/right when used with Shift key down
      pProject->TP_ScrollWindow(
         viewInfo.OffsetTimeByPixels(
            viewInfo.PositionToTime(0), 50.0 * -steps));
   }
   else if (event.CmdDown())
   {
#if 0
         // JKC: Alternative scroll wheel zooming code
         // using AudacityProject zooming, which is smarter,
         // it keeps selections on screen and centred if it can,
         // also this ensures mousewheel and zoom buttons give same result.
         double ZoomFactor = pow(2.0, steps);
         AudacityProject *p = GetProject();
         if( steps > 0 )
            p->ZoomInByFactor( ZoomFactor );
         else
            p->ZoomOutByFactor( ZoomFactor );
#endif
      // MM: Zoom in/out when used with Control key down
      // We're converting pixel positions to times,
      // counting pixels from the left edge of the track.
      int trackLeftEdge = pProject->GetTrackPanel()->GetLeftOffset();

      // Time corresponding to mouse position
      wxCoord xx;
      double center_h;
      double mouse_h = viewInfo.PositionToTime(event.m_x, trackLeftEdge);

      // Scrubbing? Expand or contract about the center, ignoring mouse position
      if (scrubber.IsScrollScrubbing())
         center_h = viewInfo.h +
            (pProject->GetTrackPanel()->GetScreenEndTime() - viewInfo.h) / 2.0;
      // Zooming out? Focus on mouse.
      else if( steps <= 0 )
         center_h = mouse_h;
      // No Selection? Focus on mouse.
      else if((viewInfo.selectedRegion.t1() - viewInfo.selectedRegion.t0() ) < 0.00001  )
         center_h = mouse_h;
      // Before Selection? Focus on left
      else if( mouse_h < viewInfo.selectedRegion.t0() )
         center_h = viewInfo.selectedRegion.t0();
      // After Selection? Focus on right
      else if( mouse_h > viewInfo.selectedRegion.t1() )
         center_h = viewInfo.selectedRegion.t1();
      // Inside Selection? Focus on mouse
      else
         center_h = mouse_h;

      xx = viewInfo.TimeToPosition(center_h, trackLeftEdge);

      // Time corresponding to last (most far right) audio.
      double audioEndTime = pProject->GetTracks()->GetEndTime();

// Disabled this code to fix Bug 1923 (tricky to wheel-zoom right of waveform).
#if 0
      // When zooming in in empty space, it's easy to 'lose' the waveform.
      // This prevents it.
      // IF zooming in
      if (steps > 0)
      {
         // IF mouse is to right of audio
         if (center_h > audioEndTime)
            // Zooming brings far right of audio to mouse.
            center_h = audioEndTime;
      }
#endif

      wxCoord xTrackEnd = viewInfo.TimeToPosition( audioEndTime );
      viewInfo.ZoomBy(pow(2.0, steps));

      double new_center_h = viewInfo.PositionToTime(xx, trackLeftEdge);
      viewInfo.h += (center_h - new_center_h);

      // If wave has gone off screen, bring it back.
      // This means that the end of the track stays where it was.
      if( viewInfo.h > audioEndTime )
         viewInfo.h += audioEndTime - viewInfo.PositionToTime( xTrackEnd );


      result |= FixScrollbars;
   }
   else
   {
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
      if (scrubber.IsScrubbing()) {
         scrubber.HandleScrollWheel(steps);
         evt.event.Skip(false);
      }
      else
#endif
      {
         // MM: Scroll up/down when used without modifier keys
         double lines = steps * 4 + mVertScrollRemainder;
         mVertScrollRemainder = lines - floor(lines);
         lines = floor(lines);
         auto didSomething = pProject->TP_ScrollUpDown((int)-lines);
         if (!didSomething)
            result |= Cancelled;
      }
   }

   return result;
}
