/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackControls.h"
#include "TrackButtonHandles.h"
#include "../../HitTestResult.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelMouseEvent.h"

int TrackControls::gCaptureState;

TrackControls::~TrackControls()
{
}

HitTestResult TrackControls::HitTest
(const TrackPanelMouseEvent &evt,
 const AudacityProject *)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   HitTestResult result;

   if (NULL != (result = CloseButtonHandle::HitTest(event, rect)).handle)
      return result;

   if (NULL != (result = MinimizeButtonHandle::HitTest(event, rect)).handle)
      return result;

   return result;
}

Track *TrackControls::FindTrack()
{
   return GetTrack();
}
