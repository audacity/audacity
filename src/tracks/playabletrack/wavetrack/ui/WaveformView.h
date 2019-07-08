/**********************************************************************

Audacity: A Digital Audio Editor

WaveformView.h

Paul Licameli split from WaveTrackView.h

**********************************************************************/

#ifndef __AUDACITY_WAVEFORM_VIEW__
#define __AUDACITY_WAVEFORM_VIEW__

#include "../../../ui/CommonTrackView.h" // to inherit

class WaveTrack;
class CutlineHandle;
class SampleHandle;
class EnvelopeHandle;

class WaveformView final : public CommonTrackView
{
   WaveformView( const WaveformView& ) = delete;
   WaveformView &operator=( const WaveformView& ) = delete;

public:
   using CommonTrackView::CommonTrackView;
   ~WaveformView() override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;


private:
   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;
   static void DoDraw(TrackPanelDrawingContext &context,
                               const WaveTrack *track,
                               const wxRect & rect,
                               bool muted);

   std::vector<UIHandlePtr> DetailedHitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject, int currentTool, bool bMultiTool )
      override;

protected:
   void DoSetMinimized( bool minimized ) override;

   std::weak_ptr<CutlineHandle> mCutlineHandle;
   std::weak_ptr<SampleHandle> mSampleHandle;
   std::weak_ptr<EnvelopeHandle> mEnvelopeHandle;
};

#endif
