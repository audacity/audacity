/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h" // for USE_* macros
#include "NoteTrackView.h"

#ifdef USE_MIDI
#include "../../../../NoteTrack.h"

#include "../../../../Experimental.h"

#include "NoteTrackControls.h"
#include "NoteTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../ui/SelectHandle.h"
#include "StretchHandle.h"

NoteTrackView::NoteTrackView( const std::shared_ptr<Track> &pTrack )
   : CommonTrackView{ pTrack }
{
   DoSetHeight( NoteTrackControls::DefaultNoteTrackHeight() );
}

NoteTrackView::~NoteTrackView()
{
}

std::vector<UIHandlePtr> NoteTrackView::DetailedHitTest
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

std::shared_ptr<TrackView> NoteTrack::DoGetView()
{
   return std::make_shared<NoteTrackView>( SharedPointer() );
}

std::shared_ptr<TrackControls> NoteTrack::DoGetControls()
{
   return std::make_shared<NoteTrackControls>( SharedPointer() );
}

std::shared_ptr<TrackVRulerControls> NoteTrackView::DoGetVRulerControls()
{
   return
      std::make_shared<NoteTrackVRulerControls>( shared_from_this() );
}

#endif
