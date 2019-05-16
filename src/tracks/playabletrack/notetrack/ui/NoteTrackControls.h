/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_CONTROLS__
#define __AUDACITY_NOTE_TRACK_CONTROLS__

#include "../../../ui/TrackControls.h"
#include "../../../../MemoryX.h"
class MuteButtonHandle;
class SoloButtonHandle;
class NoteTrackButtonHandle;
class VelocitySliderHandle;

///////////////////////////////////////////////////////////////////////////////
class NoteTrackControls : public TrackControls
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
      : TrackControls( pTrack ) {}
   ~NoteTrackControls();

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   PopupMenuTable *GetMenuExtension(Track *pTrack) override;
};

#endif
