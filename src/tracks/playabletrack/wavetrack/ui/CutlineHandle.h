/**********************************************************************

Audacity: A Digital Audio Editor

CutlineHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_CUTLINE_HANDLE__
#define __AUDACITY_CUTLINE_HANDLE__

#include "../../../../UIHandle.h"
#include "../../../../MemoryX.h"

class wxMouseEvent;
struct HitTestResult;
class WaveTrack;

class CutlineHandle final : public UIHandle
{
   CutlineHandle();
   CutlineHandle(const CutlineHandle&) = delete;
   CutlineHandle &operator=(const CutlineHandle&) = delete;
   static CutlineHandle& Instance();
   static HitTestPreview HitPreview(bool cutline, bool unsafe);

public:
   static HitTestResult HitAnywhere(const AudacityProject *pProject, bool cutline);
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
      const AudacityProject *pProject, const std::shared_ptr<WaveTrack> &pTrack);

   virtual ~CutlineHandle();

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

   bool StopsOnKeystroke() override { return true; }

private:
   enum Operation { Merge, Expand, Remove };
   Operation mOperation{ Merge };
   double mStartTime{}, mEndTime{};
   bool mbCutline{};
};

#endif
