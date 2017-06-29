/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_VZOOM_HANDLE__
#define __AUDACITY_NOTE_TRACK_VZOOM_HANDLE__

class wxMouseState;
struct HitTestResult;
class NoteTrack;

#include "../../../../MemoryX.h"
#include "../../../../UIHandle.h"
#include <wx/gdicmn.h>

class NoteTrackVZoomHandle : public UIHandle
{
   NoteTrackVZoomHandle();
   NoteTrackVZoomHandle(const NoteTrackVZoomHandle&);
   NoteTrackVZoomHandle &operator=(const NoteTrackVZoomHandle&);
   static NoteTrackVZoomHandle& Instance();
   static HitTestPreview HitPreview(const wxMouseState &state);

public:
   static HitTestResult HitTest(const wxMouseState &state);

   virtual ~NoteTrackVZoomHandle();

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

   void DrawExtras
      (DrawingPass pass,
       wxDC * dc, const wxRegion &updateRegion, const wxRect &panelRect)
      override;

private:
   std::weak_ptr<NoteTrack> mpTrack;

   int mZoomStart, mZoomEnd;
   wxRect mRect;
};

#endif
