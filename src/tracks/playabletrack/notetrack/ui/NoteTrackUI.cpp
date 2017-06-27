/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"

#ifdef USE_MIDI

#include "../../../../NoteTrack.h"
#include "NoteTrackControls.h"
#include "NoteTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../ui/SelectHandle.h"
#include "StretchHandle.h"

HitTestResult NoteTrack::DetailedHitTest
(const TrackPanelMouseEvent &event,
 const AudacityProject *pProject, int, bool )
{
   // Eligible for stretch?
   HitTestResult result;
#ifdef USE_MIDI
   StretchHandle::StretchState state;
   result = StretchHandle::HitTest( event, pProject, this, state );
#endif

   return result;
}

std::shared_ptr<TrackControls> NoteTrack::GetControls()
{
   return std::make_shared<NoteTrackControls>( Pointer( this ) );
}

std::shared_ptr<TrackVRulerControls> NoteTrack::GetVRulerControls()
{
   return std::make_shared<NoteTrackVRulerControls>( Pointer( this ) );
}
#endif
