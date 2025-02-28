/**********************************************************************

Audacity: A Digital Audio Editor

WaveChannelVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveChannelVRulerControls.h"

#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "WaveTrack.h"

#include "AColor.h"
#include "AllThemeResources.h"
#include "Theme.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../widgets/LinearUpdater.h"
#include "../../../../widgets/RealFormat.h"
#include "../../../../widgets/Ruler.h"

///////////////////////////////////////////////////////////////////////////////
Ruler& WaveChannelVRulerControls::ScratchRuler()
{
    static Ruler theRuler{
        LinearUpdater::Instance(), RealFormat::LinearInstance() };
    return theRuler;
}

void WaveChannelVRulerControls::DoDraw(ChannelVRulerControls& controls,
                                       TrackPanelDrawingContext& context,
                                       const wxRect& rect_, unsigned iPass)
{
    Ruler& vruler = ScratchRuler();

    // Draw on a later pass because the bevel overpaints one pixel
    // out of bounds on the bottom

    if (iPass == TrackArtist::PassControls) {
        auto rect = rect_;
        --rect.width;
        --rect.height;

        auto dc = &context.dc;

        // Right align the ruler
        wxRect rr = rect;
        rr.width--;

// Next code tests for a VRuler that is narrower than the rectangle
// we are drawing into.  If so, it 'right aligns' the ruler into the
// rectangle.
// However, it seems this occurs only because vrulerSize is not up to
// date.  That in turn caused Bug 2248, which was the labels being
// drawn further right than they should be (in MultiView mode).
// #ifdeffing out this code fixes bug 2248
#if 0
        if (t->vrulerSize.GetWidth() < rect.GetWidth()) {
            int adj = rr.GetWidth() - t->vrulerSize.GetWidth();
            rr.x += adj;
            rr.width -= adj;
        }
#endif

        controls.UpdateRuler(rr);

        vruler.SetTickColour(theTheme.Colour(clrTrackPanelText));
        vruler.Draw(*dc);
    }
}
