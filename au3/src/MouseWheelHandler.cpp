/**********************************************************************

Audacity: A Digital Audio Editor

MouseWheelHandler.cpp

Paul Licameli split from ProjectWindow.cpp

**********************************************************************/
#include "tracks/ui/CommonTrackPanelCell.h"
#include "RefreshCode.h"
#include "Track.h"
#include "TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "Viewport.h"
#include "tracks/ui/Scrubbing.h"

// Common mouse wheel handling in track panel cells, moved here to avoid
// compilation dependencies on Track, TrackPanel, and Scrubbing at low levels
// which made cycles
static CommonTrackPanelCell::MouseWheelHook::Scope scope{
// Need a bit of memory from one call to the next
    [mVertScrollRemainder = 0.0](
        const TrackPanelMouseEvent& evt, AudacityProject* pProject )
    mutable -> unsigned {
        using namespace RefreshCode;

        if (TrackList::Get(*pProject).empty()) {
            // Scrolling and Zoom in and out commands are disabled when there are no tracks.
            // This should be disabled too for consistency.  Otherwise
            // you do see changes in the time ruler.
            return Cancelled;
        }

        unsigned result = RefreshAll;
        const wxMouseEvent& event = evt.event;
        auto& viewInfo = ViewInfo::Get(*pProject);
        Scrubber& scrubber = Scrubber::Get(*pProject);
        auto& viewport = Viewport::Get(*pProject);
        const auto steps = evt.steps;

        if (event.ShiftDown()
            // Don't pan during smooth scrolling.  That would conflict with keeping
            // the play indicator centered.
            && !scrubber.IsScrollScrubbing()
            ) {
            // MM: Scroll left/right when used with Shift key down
            viewport.SetHorizontalThumb(
                viewInfo.OffsetTimeByPixels(
                    viewInfo.PositionToTime(0), 50.0 * -steps));
        } else if (event.CmdDown()) {
#if 0
            // JKC: Alternative scroll wheel zooming code
            // using AudacityProject zooming, which is smarter,
            // it keeps selections on screen and centred if it can,
            // also this ensures mousewheel and zoom buttons give same result.
            double ZoomFactor = pow(2.0, steps);
            AudacityProject* p = GetProject();
            if (steps > 0) {
                // PRL:  Track panel refresh may be needed if you reenable this
                // code, but we don't want this file dependent on TrackPanel.cpp
                p->ZoomAboutSelection(ZoomFactor);
            } else {
                p->ZoomAboutCenter(ZoomFactor);
            }
#endif
            // MM: Zoom in/out when used with Control key down
            // We're converting pixel positions to times,
            // counting pixels from the left edge of the track.
            int trackLeftEdge = viewInfo.GetLeftOffset();

            // Time corresponding to mouse position
            wxCoord xx;
            double center_h;
            double mouse_h = viewInfo.PositionToTime(event.m_x, trackLeftEdge);

            // Scrubbing? Expand or contract about the center, ignoring mouse position
            if (scrubber.IsScrollScrubbing()) {
                center_h = viewInfo.hpos
                           + (viewInfo.GetScreenEndTime() - viewInfo.hpos) / 2.0;
            }
            // Zooming out? Focus on mouse.
            else if (steps <= 0) {
                center_h = mouse_h;
            }
            // No Selection? Focus on mouse.
            else if ((viewInfo.selectedRegion.t1() - viewInfo.selectedRegion.t0()) < 0.00001) {
                center_h = mouse_h;
            }
            // Before Selection? Focus on left
            else if (mouse_h < viewInfo.selectedRegion.t0()) {
                center_h = viewInfo.selectedRegion.t0();
            }
            // After Selection? Focus on right
            else if (mouse_h > viewInfo.selectedRegion.t1()) {
                center_h = viewInfo.selectedRegion.t1();
            }
            // Inside Selection? Focus on mouse
            else {
                center_h = mouse_h;
            }

            xx = viewInfo.TimeToPosition(center_h, trackLeftEdge);

            // Time corresponding to last (most far right) audio.
            double audioEndTime = TrackList::Get(*pProject).GetEndTime();

// Disabled this code to fix Bug 1923 (tricky to wheel-zoom right of waveform).
#if 0
            // When zooming in empty space, it's easy to 'lose' the waveform.
            // This prevents it.
            // IF zooming in
            if (steps > 0) {
                // IF mouse is to right of audio
                if (center_h > audioEndTime) {
                    // Zooming brings far right of audio to mouse.
                    center_h = audioEndTime;
                }
            }
#endif

            wxCoord xTrackEnd = viewInfo.TimeToPosition(audioEndTime);
            viewInfo.ZoomBy(pow(2.0, steps / 4.0));

            double new_center_h = viewInfo.PositionToTime(xx, trackLeftEdge);
            viewInfo.hpos += (center_h - new_center_h);

            // If wave has gone off screen, bring it back.
            // This means that the end of the track stays where it was.
            if (viewInfo.hpos > audioEndTime) {
                viewInfo.hpos += audioEndTime - viewInfo.PositionToTime(xTrackEnd);
            }

            result |= FixScrollbars;
        } else {
            if (scrubber.IsScrubbing()) {
                scrubber.HandleScrollWheel(steps);
                evt.event.Skip(false);
            } else {
                // MM: Scroll up/down when used without modifier keys
                double lines = steps * 4 + mVertScrollRemainder;
                mVertScrollRemainder = lines - floor(lines);
                lines = floor(lines);
                auto didSomething = viewport.ScrollUpDown((int)-lines);
                if (!didSomething) {
                    result |= Cancelled;
                }
            }
        }

        return result;
    } };
