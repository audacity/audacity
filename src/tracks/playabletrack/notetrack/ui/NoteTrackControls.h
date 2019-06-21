/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_CONTROLS__
#define __AUDACITY_NOTE_TRACK_CONTROLS__

#include "../../ui/PlayableTrackControls.h" // to inherit
class wxEvent;
class LWSlider;
class NoteTrack;
class MuteButtonHandle;
class SoloButtonHandle;
class NoteTrackButtonHandle;
class VelocitySliderHandle;

#include "../../../../Audacity.h"
#include "../../../../Experimental.h"

using NoteTrackControlsBase =
#ifdef EXPERIMENTAL_MIDI_OUT
   PlayableTrackControls
#else
   CommonTrackControls
#endif
   ;

///////////////////////////////f////////////////////////////////////////////////
class NoteTrackControls : public NoteTrackControlsBase
{
   NoteTrackControls(const NoteTrackControls&) = delete;
   NoteTrackControls &operator=(const NoteTrackControls&) = delete;

   std::weak_ptr<MuteButtonHandle> mMuteHandle;
   std::weak_ptr<SoloButtonHandle> mSoloHandle;
   std::weak_ptr<NoteTrackButtonHandle> mClickHandle;
   std::weak_ptr<VelocitySliderHandle> mVelocityHandle;

public:
   explicit
   NoteTrackControls( std::shared_ptr<Track> pTrack )
      : NoteTrackControlsBase( pTrack ) {}
   ~NoteTrackControls();

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   PopupMenuTable *GetMenuExtension(Track *pTrack) override;

   const TCPLines& GetTCPLines() const override;

   static unsigned DefaultNoteTrackHeight();
   static void GetMidiControlsRect(const wxRect & rect, wxRect & dest);
   static void GetVelocityRect(const wxPoint &topleft, wxRect & dest);
   
   static LWSlider * VelocitySlider
      (const wxRect &sliderRect, const NoteTrack *t, bool captured,
       wxWindow *pParent);

private:
   static void ReCreateVelocitySlider( wxEvent& );
};

#endif
