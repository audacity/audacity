/**********************************************************************

Audacity: A Digital Audio Editor

EnvelopeHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_ENVELOPE_HANDLE__
#define __AUDACITY_ENVELOPE_HANDLE__

#include "../../UIHandle.h"
#include "../../MemoryX.h"

class wxMouseEvent;
#include <wx/gdicmn.h>

class Envelope;
class EnvelopeEditor;
struct HitTestResult;
class ViewInfo;
class TimeTrack;
class WaveTrack;

class EnvelopeHandle final : public UIHandle
{
   EnvelopeHandle();
   EnvelopeHandle(const EnvelopeHandle&) = delete;
   EnvelopeHandle &operator=(const EnvelopeHandle&) = delete;
   static EnvelopeHandle& Instance();
   static HitTestPreview HitPreview(const AudacityProject *pProject, bool unsafe);

   static HitTestResult HitEnvelope
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject,
       const Envelope *envelope, float zoomMin, float zoomMax,
       bool dB, float dBRange);

public:
   static HitTestResult HitAnywhere(const AudacityProject *pProject);
   static HitTestResult TimeTrackHitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject, const std::shared_ptr<TimeTrack> &tt);
   static HitTestResult WaveTrackHitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject, const std::shared_ptr<WaveTrack> &wt);

   virtual ~EnvelopeHandle();

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
   bool ForwardEventToEnvelopes
      (const wxMouseEvent &event, const ViewInfo &viewInfo);

   wxRect mRect{};
   bool mLog{};
   float mLower{}, mUpper{};
   double mdBRange{};

   std::unique_ptr<EnvelopeEditor> mEnvelopeEditor;
   std::unique_ptr<EnvelopeEditor> mEnvelopeEditorRight;
};

#endif
