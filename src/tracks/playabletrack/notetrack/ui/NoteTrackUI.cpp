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

std::vector<UIHandlePtr> NoteTrack::DetailedHitTest
(const TrackPanelMouseState &state,
 const AudacityProject *pProject, int, bool )
{
   // Eligible for stretch?
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
#ifdef USE_MIDI
   result = StretchHandle::HitTest(
      mStretchHandle, state, pProject, Pointer<NoteTrack>(this) );
   if (result)
      results.push_back(result);
#endif

   return results;
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
