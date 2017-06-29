/**********************************************************************

Audacity: A Digital Audio Editor

ZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_ZOOM_HANDLE__
#define __AUDACITY_ZOOM_HANDLE__

#include "../../UIHandle.h"

class wxMouseState;
#include <wx/gdicmn.h>

struct HitTestResult;

class ZoomHandle final : public UIHandle
{
   ZoomHandle();
   ZoomHandle(const ZoomHandle&) = delete;
   ZoomHandle &operator=(const ZoomHandle&) = delete;
   static ZoomHandle& Instance();
   static HitTestPreview HitPreview
      (const wxMouseState &state, const AudacityProject *pProject);

public:
   static HitTestResult HitAnywhere
      (const wxMouseState &state, const AudacityProject *pProject);
   static HitTestResult HitTest
      (const wxMouseState &state, const AudacityProject *pProject);

   virtual ~ZoomHandle();

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
   bool IsDragZooming() const;

   int mZoomStart{}, mZoomEnd{};
   wxRect mRect{};
};

#endif
