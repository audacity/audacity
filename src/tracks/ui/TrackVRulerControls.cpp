/**********************************************************************

Audacity: A Digital Audio Editor

TrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "../../HitTestResult.h"
#include "TrackVRulerControls.h"

#include <wx/cursor.h>
#include <wx/translation.h>

TrackVRulerControls::~TrackVRulerControls()
{
}

Track *TrackVRulerControls::FindTrack()
{
   return GetTrack();
}

HitTestResult TrackVRulerControls::HitTest
   (const TrackPanelMouseEvent &event, const AudacityProject *pProject)
{
   // Use a space for the tip, otherwise we get the default message.
   static wxCursor arrowCursor{ wxCURSOR_ARROW };
   return { { _(" "), &arrowCursor }, nullptr };
}