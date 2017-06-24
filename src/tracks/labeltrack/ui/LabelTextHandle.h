/**********************************************************************

Audacity: A Digital Audio Editor

LabelTextHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_TEXT_HANDLE__
#define __AUDACITY_LABEL_TEXT_HANDLE__

#include "../../../UIHandle.h"
#include "../../../MemoryX.h"
#include "../../../SelectedRegion.h"
#include <wx/gdicmn.h>

class wxMouseEvent;
struct HitTestResult;
class LabelTrack;
class SelectionStateChanger;

class LabelTextHandle final : public UIHandle
{
   LabelTextHandle();
   LabelTextHandle(const LabelTextHandle&) = delete;
   LabelTextHandle &operator=(const LabelTextHandle&) = delete;
   static LabelTextHandle& Instance();

public:
   static HitTestResult HitTest(const wxMouseEvent &event, LabelTrack *pLT);

   virtual ~LabelTextHandle();

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

private:
   std::weak_ptr<LabelTrack> mpLT {};
   int mLabelTrackStartXPos { -1 };
   int mLabelTrackStartYPos { -1 };
   SelectedRegion mSelectedRegion{};
   std::unique_ptr<SelectionStateChanger> mChanger;
};

#endif
