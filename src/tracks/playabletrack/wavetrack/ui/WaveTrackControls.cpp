/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackControls.h"
#include "../../ui/PlayableTrackButtonHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../Track.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelMouseEvent.h"

WaveTrackControls::WaveTrackControls()
{
}

WaveTrackControls &WaveTrackControls::Instance()
{
   static WaveTrackControls instance;
   return instance;
}

WaveTrackControls::~WaveTrackControls()
{
}


HitTestResult WaveTrackControls::HitTest
(const TrackPanelMouseEvent & evt,
 const AudacityProject *pProject)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   if (event.Button(wxMOUSE_BTN_LEFT)) {
      if (mpTrack->GetKind() == Track::Wave) {
         auto track = GetTrack();
         HitTestResult result;
         if (NULL !=
             (result = MuteButtonHandle::HitTest
                 (event, rect, pProject, track)).handle)
            return result;

         if (NULL !=
             (result = SoloButtonHandle::HitTest
                 (event, rect, pProject, track)).handle)
            return result;
      }
   }

   return TrackControls::HitTest(evt, pProject);
}
