/**********************************************************************

Audacity: A Digital Audio Editor

LabelTextHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_TEXT_HANDLE__
#define __AUDACITY_LABEL_TEXT_HANDLE__

#include "LabelDefaultClickHandle.h"
#include "../../../MemoryX.h"
#include "../../../SelectedRegion.h"
#include <wx/gdicmn.h>

class wxMouseState;
struct HitTestResult;
class LabelTrack;
class SelectionStateChanger;

class LabelTextHandle final : public LabelDefaultClickHandle
{
   LabelTextHandle();
   LabelTextHandle(const LabelTextHandle&) = delete;
   LabelTextHandle &operator=(const LabelTextHandle&) = delete;
   static LabelTextHandle& Instance();

   static HitTestPreview HitPreview();

public:
   static HitTestResult HitTest(
      const wxMouseState &state, const std::shared_ptr<LabelTrack> &pLT);

   virtual ~LabelTextHandle();

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

private:
   std::weak_ptr<LabelTrack> mpLT {};
   int mLabelTrackStartXPos { -1 };
   int mLabelTrackStartYPos { -1 };
   SelectedRegion mSelectedRegion{};
   std::unique_ptr<SelectionStateChanger> mChanger;
};

#endif
