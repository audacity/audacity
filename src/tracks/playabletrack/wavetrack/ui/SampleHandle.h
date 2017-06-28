/**********************************************************************

Audacity: A Digital Audio Editor

SampleHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_HANDLE__
#define __AUDACITY_SAMPLE_HANDLE__

#include "../../../../UIHandle.h"
#include "audacity/Types.h"
#include "../../../../MemoryX.h"

class wxMouseEvent;
#include <wx/gdicmn.h>

struct HitTestResult;
class Track;
class ViewInfo;
class WaveTrack;

class SampleHandle final : public UIHandle
{
   SampleHandle();
   SampleHandle(const SampleHandle&) = delete;
   SampleHandle &operator=(const SampleHandle&) = delete;
   static SampleHandle& Instance();
   static HitTestPreview HitPreview
      (const wxMouseEvent &event, const AudacityProject *pProject, bool unsafe);

public:
   static HitTestResult HitAnywhere
      (const wxMouseEvent &event, const AudacityProject *pProject);
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject, const std::shared_ptr<WaveTrack> &pTrack);

   virtual ~SampleHandle();

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
   float FindSampleEditingLevel
      (const wxMouseEvent &event, const ViewInfo &viewInfo, double t0);

   std::shared_ptr<WaveTrack> mClickedTrack;
   wxRect mRect{};

   sampleCount mClickedStartSample{};
   sampleCount mLastDragSample{};
   float mLastDragSampleValue{};
   bool mAltKey{};
};

#endif
