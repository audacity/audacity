/**********************************************************************

Sneedacity: A Digital Audio Editor

SampleHandle.h

Paul Licameli

**********************************************************************/

#ifndef __SNEEDACITY_SAMPLE_HANDLE__
#define __SNEEDACITY_SAMPLE_HANDLE__

#include "../../../../UIHandle.h"
#include "sneedacity/Types.h"

class wxMouseEvent;
class wxMouseState;

class Track;
class ViewInfo;
class WaveTrack;

class SampleHandle final : public UIHandle
{
   SampleHandle(const SampleHandle&) = delete;
   static HitTestPreview HitPreview
      (const wxMouseState &state, const SneedacityProject *pProject, bool unsafe);

public:
   explicit SampleHandle( const std::shared_ptr<WaveTrack> &pTrack );

   SampleHandle &operator=(const SampleHandle&) = default;

   static UIHandlePtr HitAnywhere
      (std::weak_ptr<SampleHandle> &holder,
       const wxMouseState &state, const std::shared_ptr<WaveTrack> &pTrack);
   static UIHandlePtr HitTest
      (std::weak_ptr<SampleHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const SneedacityProject *pProject, const std::shared_ptr<WaveTrack> &pTrack);

   virtual ~SampleHandle();

   std::shared_ptr<WaveTrack> GetTrack() const { return mClickedTrack; }

   void Enter(bool forward, SneedacityProject *) override;

   Result Click
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, SneedacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(SneedacityProject *pProject) override;

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
