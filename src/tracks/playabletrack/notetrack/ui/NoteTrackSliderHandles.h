/**********************************************************************

 Audacity: A Digital Audio Editor

 NoteTrackSliderHandles.h

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_SLIDER_HANDLES__
#define __AUDACITY_NOTE_TRACK_SLIDER_HANDLES__

#include "../../../../Experimental.h"

#ifdef EXPERIMENTAL_MIDI_OUT

#include "../../../ui/SliderHandle.h"

class NoteTrack;
class wxMouseState;

struct HitTestResult;

class VelocitySliderHandle final : public SliderHandle
{
   VelocitySliderHandle(const VelocitySliderHandle&) = delete;
   VelocitySliderHandle &operator=(const VelocitySliderHandle&) = delete;

   VelocitySliderHandle();
   virtual ~VelocitySliderHandle();
   static VelocitySliderHandle& Instance();

   std::shared_ptr<NoteTrack> GetNoteTrack();

protected:
   float GetValue() override;
   Result SetValue
   (AudacityProject *pProject, float newValue) override;
   Result CommitChanges
   (const wxMouseEvent &event, AudacityProject *pProject) override;

   bool StopsOnKeystroke () override { return true; }

public:
   static HitTestResult HitTest
   (const wxMouseState &state, const wxRect &rect,
    const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack);
};

#endif

#endif
