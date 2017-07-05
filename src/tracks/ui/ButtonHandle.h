/**********************************************************************

Audacity: A Digital Audio Editor

ButtonHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_BUTTON_HANDLE__
#define __AUDACITY_BUTTON_HANDLE__

#include "../../UIHandle.h"
#include "../../MemoryX.h"

class wxMouseEvent;
#include <wx/gdicmn.h>

class Track;

class ButtonHandle /* not final */ : public UIHandle
{
   ButtonHandle(const ButtonHandle&) = delete;

protected:
   explicit ButtonHandle
      ( const std::shared_ptr<Track> &pTrack, const wxRect &rect, int dragCode );

   ButtonHandle &operator=(const ButtonHandle&) = default;

   virtual ~ButtonHandle();

   // This new abstract virtual simplifies the duties of further subclasses.
   // This class will decide whether to refresh the clicked cell for button state
   // change.
   // Subclass can decide to refresh other things and the results will be ORed.
   virtual Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent) = 0;

   // For derived class to define hit tests
   static HitTestPreview HitPreview();

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

   std::weak_ptr<Track> mpTrack;
   wxRect mRect;
   int mDragCode;
};

#endif
