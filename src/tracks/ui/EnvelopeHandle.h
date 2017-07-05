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
class wxMouseState;
#include <wx/gdicmn.h>

class Envelope;
class EnvelopeEditor;
struct HitTestResult;
class ViewInfo;
class TimeTrack;
class WaveTrack;

class EnvelopeHandle final : public UIHandle
{
   EnvelopeHandle(const EnvelopeHandle&) = delete;
   EnvelopeHandle &operator=(const EnvelopeHandle&) = delete;
   static HitTestPreview HitPreview(const AudacityProject *pProject, bool unsafe);

   static HitTestResult HitEnvelope
      (std::weak_ptr<EnvelopeHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const AudacityProject *pProject,
       Envelope *envelope, float zoomMin, float zoomMax,
       bool dB, float dBRange);

public:
   explicit EnvelopeHandle( Envelope *pEnvelope );

   virtual ~EnvelopeHandle();

   static HitTestResult HitAnywhere
      (std::weak_ptr<EnvelopeHandle> &holder,
       const AudacityProject *pProject, Envelope *envelope);
   static HitTestResult TimeTrackHitTest
      (std::weak_ptr<EnvelopeHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const AudacityProject *pProject, const std::shared_ptr<TimeTrack> &tt);
   static HitTestResult WaveTrackHitTest
      (std::weak_ptr<EnvelopeHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const AudacityProject *pProject, const std::shared_ptr<WaveTrack> &wt);

   Envelope *GetEnvelope() const { return mEnvelope; }

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

   bool StopsOnKeystroke() override { return true; }

private:
   bool ForwardEventToEnvelopes
      (const wxMouseEvent &event, const ViewInfo &viewInfo);

   wxRect mRect{};
   bool mLog{};
   float mLower{}, mUpper{};
   double mdBRange{};

   Envelope *mEnvelope{};
   std::unique_ptr<EnvelopeEditor> mEnvelopeEditor;
   std::unique_ptr<EnvelopeEditor> mEnvelopeEditorRight;
};

#endif
