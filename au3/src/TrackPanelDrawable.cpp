/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelDrawable.cpp

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#include "TrackPanelDrawable.h"

TrackPanelDrawable::~TrackPanelDrawable()
{
}

void TrackPanelDrawable::Draw(
    TrackPanelDrawingContext&, const wxRect&, unsigned)
{
}

wxRect TrackPanelDrawable::DrawingArea(
    TrackPanelDrawingContext&,
    const wxRect& rect, const wxRect&, unsigned)
{
    return rect;
}
