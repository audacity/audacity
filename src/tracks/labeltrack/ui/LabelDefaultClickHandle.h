/**********************************************************************

Audacity: A Digital Audio Editor

LabelDefaultClickHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_DEFAULT_CLICK_HANDLE__
#define __AUDACITY_LABEL_DEFAULT_CLICK_HANDLE__

#include "../../../UIHandle.h"
#include "../../../MemoryX.h"

class wxMouseEvent;
struct HitTestResult;
class LabelTrack;

// Adds some behavior to click, then calls through to other mouse handling.
class LabelDefaultClickHandle final : public UIHandle
{
   LabelDefaultClickHandle();
   LabelDefaultClickHandle(const LabelDefaultClickHandle&) = delete;
   LabelDefaultClickHandle &operator=(const LabelDefaultClickHandle&) = delete;

public:
   static LabelDefaultClickHandle& Instance();
   virtual ~LabelDefaultClickHandle();

   void DoClick
      (const wxMouseEvent &event, AudacityProject *pProject, TrackPanelCell *pCell);

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseEvent &event, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   void DrawExtras
      (DrawingPass pass,
       wxDC * dc, const wxRegion &updateRegion, const wxRect &panelRect)
       override;

   bool StopsOnKeystroke() override;

   void OnProjectChange(AudacityProject *pProject) override;

   UIHandle *mpForward {};

private:
   struct LabelState;
   std::unique_ptr< LabelState > mLabelState;
   void SaveState( AudacityProject *pProject );
   void RestoreState( AudacityProject *pProject );
};

#endif
