/**********************************************************************

Sneedacity: A Digital Audio Editor

EnvelopeHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_ENVELOPE_HANDLE__
#define __SNEEDACITY_ENVELOPE_HANDLE__

#include "../../UIHandle.h"

#include <vector>

class wxMouseEvent;
class wxMouseState;

class Envelope;
class EnvelopeEditor;
class ViewInfo;
class TimeTrack;
class WaveTrack;

class SNEEDACITY_DLL_API EnvelopeHandle final : public UIHandle
{
   EnvelopeHandle(const EnvelopeHandle&) = delete;
   EnvelopeHandle &operator=(const EnvelopeHandle&) = delete;

   static UIHandlePtr HitEnvelope
      (std::weak_ptr<EnvelopeHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const SneedacityProject *pProject,
       Envelope *envelope, float zoomMin, float zoomMax,
       bool dB, float dBRange, bool timeTrack);

public:
   explicit EnvelopeHandle( Envelope *pEnvelope );

   virtual ~EnvelopeHandle();

   static UIHandlePtr HitAnywhere
      (std::weak_ptr<EnvelopeHandle> &holder, Envelope *envelope,
       bool timeTrack);
   static UIHandlePtr TimeTrackHitTest
      (std::weak_ptr<EnvelopeHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const SneedacityProject *pProject, const std::shared_ptr<TimeTrack> &tt);
   static UIHandlePtr WaveTrackHitTest
      (std::weak_ptr<EnvelopeHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const SneedacityProject *pProject, const std::shared_ptr<WaveTrack> &wt);

   Envelope *GetEnvelope() const { return mEnvelope; }

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
   bool ForwardEventToEnvelopes
      (const wxMouseEvent &event, const ViewInfo &viewInfo);

   wxRect mRect{};
   bool mLog{};
   float mLower{}, mUpper{};
   double mdBRange{};

   Envelope *mEnvelope{};
   std::vector< std::unique_ptr<EnvelopeEditor> > mEnvelopeEditors;

   bool mTimeTrack{};
};

#endif
