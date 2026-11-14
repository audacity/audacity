/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "LabelTrackVRulerControls.h"

#include "../../../HitTestResult.h"

#include "AColor.h"
#include "../../../TrackArtist.h"
#include "../../../TrackPanelDrawingContext.h"

LabelTrackVRulerControls::~LabelTrackVRulerControls()
{
}

void LabelTrackVRulerControls::Draw(
    TrackPanelDrawingContext& context,
    const wxRect& rect_, unsigned iPass)
{
    ChannelVRulerControls::Draw(context, rect_, iPass);
}

void LabelTrackVRulerControls::UpdateRuler(const wxRect& rect)
{
    // Label tracks do not have a vruler
    // do nothing
}
