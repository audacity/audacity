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
(const TrackPanelMouseState &state,
 const AudacityProject *pProject, int, bool )
{
   // Eligible for stretch?
   HitTestResult result;
#ifdef USE_MIDI
   result = StretchHandle::HitTest(
      mStretchHandle, state, pProject, Pointer<NoteTrack>(this) );
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
