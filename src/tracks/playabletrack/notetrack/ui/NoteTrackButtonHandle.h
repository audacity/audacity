/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackButtonHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_BUTTON_HANDLE__
#define __AUDACITY_NOTE_TRACK_BUTTON_HANDLE__

class wxMouseState;
class NoteTrack;
struct HitTestResult;

#include <wx/gdicmn.h>
#include "../../../../MemoryX.h"
#include "../../../../UIHandle.h"

///////////////////////////////////////////////////////////////////////////////
// TODO: do appearance changes as in ButtonHandle, or even, inherit from that
class NoteTrackButtonHandle : public UIHandle
{
   NoteTrackButtonHandle(const NoteTrackButtonHandle&);
   NoteTrackButtonHandle &operator=(const NoteTrackButtonHandle&);
   NoteTrackButtonHandle();
   virtual ~NoteTrackButtonHandle();
   static NoteTrackButtonHandle& Instance();

public:
   static HitTestResult HitTest
   (const wxMouseState &state, const wxRect &rect,
    const std::shared_ptr<NoteTrack> &pTrack);

protected:
   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   std::weak_ptr<NoteTrack> mpTrack;
   wxRect mRect{};
};

#endif
