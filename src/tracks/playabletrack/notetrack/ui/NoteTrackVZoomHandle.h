/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_VZOOM_HANDLE__
#define __AUDACITY_NOTE_TRACK_VZOOM_HANDLE__

class wxMouseState;
class NoteTrack;

#include "../../../../MemoryX.h"
#include "../../../../UIHandle.h"
#include <wx/gdicmn.h>

class NoteTrackVZoomHandle : public UIHandle
{
   NoteTrackVZoomHandle(const NoteTrackVZoomHandle&);
   static HitTestPreview HitPreview(const wxMouseState &state);

public:
   explicit NoteTrackVZoomHandle
      (const std::shared_ptr<NoteTrack> &pTrack, const wxRect &rect, int y);

   NoteTrackVZoomHandle &operator=(const NoteTrackVZoomHandle&) = default;

   static UIHandlePtr HitTest
      (std::weak_ptr<NoteTrackVZoomHandle> &holder,
       const wxMouseState &state,
       const std::shared_ptr<NoteTrack> &pTrack, const wxRect &rect);

   virtual ~NoteTrackVZoomHandle();

   std::shared_ptr<NoteTrack> GetTrack() const { return mpTrack.lock(); }

   void Enter(bool forward) override;

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
