//
//  Overlay.h
//  Audacity
//
//  Created by Paul Licameli on 5/7/16.
//
//

#ifndef __AUDACITY_OVERLAY__
#define __AUDACITY_OVERLAY__

#include <utility>

class OverlayPanel;
class wxDC;
class wxRect;
class wxSize;

/*
<b>How Audacity Redisplay Works \n
 Roger Dannenberg</b> \n
Oct 2010 \n

This is a brief guide to Audacity redisplay -- it may not be complete. It
is my attempt to understand the complicated graphics strategy.

One basic idea is that redrawing waveforms is rather slow, so Audacity
saves waveform images in bitmaps to make redrawing faster. In particular,
during audio playback (and recording), the vertical time indicator is
drawn over the waveform about 20 times per second. To avoid unnecessary
computation, the indicator is erased by copying a column of pixels from
a bitmap image of the waveform. Notice that this implies a two-stage
process: first, waveforms are drawn to the bitmap; then, the bitmap
(or pieces of it) are copied to the screen, perhaps along with other
graphics.

The bitmap is for the entire track panel, i.e. multiple tracks, and
includes things like the Gain and Pan slders to the left of the
waveform images.

The screen update uses a mixture of direct drawing and indirect paint
events. The "normal" way to update a graphical display is to call
the Refresh() method when something invalidates the screen. Later, the
system calls OnPaint(), which the application overrides to (re)draw the
screen. In wxWidgets, you can also draw directly to the screen without
calling Refresh() and without waiting for OnPaint() to be called.

I would expect there to be a 2-level invalidation scheme: Some changes
invalidate the bitmap, forcing a bitmap redraw *and* a screen redraw.
Other changes merely update the screen using pre-existing bitmaps. In
Audacity, the "2-level" invalidation works like this: Anything
that invalidates the bitmap calls TrackPanel::Refresh(), which
has an eraseBackground parameter. This flag says to redraw the
bitmap when OnPaint() is called. If eraseBackground is false, the
existing bitmap can be used for waveform images. Audacity also
draws directly to the screen to update the time indicator during
playback. To move the indicator, one column of pixels is drawn to
the screen to remove the indicator. Then the indicator is drawn at
a NEW time location.

Notice that the zoom guidelines, the focused track highlight,
and snap guidelines could be drawn directly to the screen rather than to
the bitmap, generally eliminating redraw work.

One problem is slider updates. Sliders are in the left area of the track
panel. They are not wxWindows like wxSliders, but instead are just drawn
on the TrackPanel. When slider state changes, *all* tracks do a full
refresh, including recomputing the backing store. It would make more sense
to just invalidate the region containing the slider. However, doing that
would require either incrementally updating the bitmap (not currently done),
or maintaining the sliders and other track info on the screen and not in
the bitmap.

*/

/*
PRL: above explanation was formerly in TrackArtist.cpp.

What it says is correct but also applies to other things than the play
indicator, such as the point editing cursor and the quick-play indicator
line.  I abstracted out class Overlay to describe these drawables, and
cooperating class OverlayPanel that can manage multiple Overlays, figuring
out when the invalidation of one of them necessitates invalidation of other
overlapping ones.

The base class OverlayPanel, of TrackPanel, was also reused by the
AdornedRulerPanel.

*/

class AUDACITY_DLL_API Overlay
{
public:
    Overlay() = default;
    Overlay(const Overlay&) = delete;
    Overlay& operator=(const Overlay&) = delete;
    virtual ~Overlay() = 0;

    ///\brief This number determines an ordering of overlays, so that those
    /// with higher numbers overpaint those with lower numbers that intersect
    virtual unsigned SequenceNumber() const = 0;

    // nonvirtual wrapper
    std::pair<wxRect, bool> GetRectangle(wxSize size);

    // size passes the dimensions of the backing dc
    // First member of pair is the rectangle that would be erased
    // Second member of pair indicates whether the overlay is out of date
    virtual std::pair<wxRect, bool> DoGetRectangle(wxSize size) = 0;

    // Default implementation blits from backing store over GetRectangle().first
    virtual void Erase(wxDC& dc, wxDC& src);

    // Draw; dc.GetSize() tells you the total dimensions, and the panel is supplied
    // as context
    virtual void Draw(OverlayPanel& panel, wxDC& dc) = 0;
};

#endif
