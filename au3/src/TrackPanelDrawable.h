/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelDrawable.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_DRAWABLE__
#define __AUDACITY_TRACK_PANEL_DRAWABLE__

#include <wx/gdicmn.h> // for wxRect

struct TrackPanelDrawingContext;
class wxRect;

/// \brief Drawing interface common to cells, groups of cells, and temporary handles in CellularPanel
class AUDACITY_DLL_API TrackPanelDrawable
{
public:
    virtual ~TrackPanelDrawable() = 0;

    // Drawing functions of the subdivision are visited repeatedly in one
    // painting if their areas (as computed by DrawingArea) intersect the
    // panel area, and depending on iPass may overpaint results of previous
    // passes.
    // Drawing function is given the rectangle computed by DrawingArea.
    // iPass counts zero-based up to some limit given to CellularPanel::Draw.
    // Default implementation does nothing.
    virtual void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass);

    // For drawing purposes, a cell might require a bigger rectangle than for
    // hit-test purposes, spilling over into other parts of the partition of the
    // panel area.
    // Default implementation returns rect unchanged.
    // TrackPanelContext is passed in because sometimes a drawing context is
    // needed for text extent calculations.
    virtual wxRect DrawingArea(
        TrackPanelDrawingContext& context, const wxRect& rect, const wxRect& panelRect, unsigned iPass);

    // Utilities for implementing DrawingArea:
    static inline wxRect MaximizeWidth(
        const wxRect& rect, const wxRect& panelRect)
    {
        return { panelRect.x, rect.y, panelRect.width, rect.height };
    }

    static inline wxRect MaximizeHeight(
        const wxRect& rect, const wxRect& panelRect)
    {
        return { rect.x, panelRect.y, rect.width, panelRect.height };
    }
};

#endif
