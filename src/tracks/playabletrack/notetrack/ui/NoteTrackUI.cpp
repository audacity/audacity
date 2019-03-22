/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackUI.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h" // for USE_* macros
#ifdef USE_MIDI
#include "../../../../NoteTrack.h"

#include "../../../../Experimental.h"

#include "NoteTrackControls.h"
#include "NoteTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../Project.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../ui/SelectHandle.h"
#include "StretchHandle.h"

std::vector<UIHandlePtr> NoteTrack::DetailedHitTest
(const TrackPanelMouseState &WXUNUSED(state),
 const AudacityProject *WXUNUSED(pProject), int, bool )
{
   // Eligible for stretch?
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
#ifdef USE_MIDI
#ifdef EXPERIMENTAL_MIDI_STRETCHING
   result = StretchHandle::HitTest(
      mStretchHandle, state, pProject, Pointer<NoteTrack>(this) );
   if (result)
      results.push_back(result);
#endif
#endif

   return results;
}

std::shared_ptr<TrackControls> NoteTrack::DoGetControls()
{
   return std::make_shared<NoteTrackControls>( SharedPointer() );
}

std::shared_ptr<TrackVRulerControls> NoteTrack::DoGetVRulerControls()
{
   return std::make_shared<NoteTrackVRulerControls>( SharedPointer() );
}
#endif
