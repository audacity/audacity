/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"

#ifdef USE_MIDI

#include "NoteTrackControls.h"
#include "../../ui/PlayableTrackButtonHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../Track.h"
#include "../../../../TrackPanelMouseEvent.h"

NoteTrackControls::NoteTrackControls()
{
}

NoteTrackControls &NoteTrackControls::Instance()
{
   static NoteTrackControls instance;
   return instance;
}

NoteTrackControls::~NoteTrackControls()
{
}

HitTestResult NoteTrackControls::HitTest
(const TrackPanelMouseEvent & evt,
 const AudacityProject *pProject)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   if (event.Button(wxMOUSE_BTN_LEFT)) {
      if (mpTrack->GetKind() == Track::Note) {
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

#endif