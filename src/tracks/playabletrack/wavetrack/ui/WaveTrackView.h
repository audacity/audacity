/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.h

Paul Licameli split from class WaveTrack

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VIEW__
#define __AUDACITY_WAVE_TRACK_VIEW__

#include "../../../ui/CommonTrackView.h"

class WaveTrack;

class WaveTrackView final : public CommonTrackView
{
   WaveTrackView( const WaveTrackView& ) = delete;
   WaveTrackView &operator=( const WaveTrackView& ) = delete;

public:
   explicit
   WaveTrackView( const std::shared_ptr<Track> &pTrack );
   ~WaveTrackView() override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;


   // CommonTrackView implementation
   void Reparent( const std::shared_ptr<Track> &parent ) override;

private:
   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;
   static std::pair<
      bool, // if true, hit-testing is finished
      std::vector<UIHandlePtr>
   > DoDetailedHitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject, int currentTool, bool bMultiTool,
      const std::shared_ptr<WaveTrack> &wt,
      CommonTrackView &view);

protected:
   void DoSetMinimized( bool minimized ) override;

   std::shared_ptr< CommonTrackView > mWaveformView, mSpectrumView;

   friend class SpectrumView;
   friend class WaveformView;
};

// Helper for drawing routines
class SelectedRegion;
class WaveClip;
class ZoomInfo;

struct ClipParameters
{
   // Do a bunch of calculations common to waveform and spectrum drawing.
   ClipParameters
      (bool spectrum, const WaveTrack *track, const WaveClip *clip, const wxRect &rect,
      const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo);

   double tOffset;
   double rate;
   double h; // absolute time of left edge of display
   double tpre; // offset corrected time of left edge of display
   double h1;
   double tpost; // offset corrected time of right edge of display

   // Calculate actual selection bounds so that t0 > 0 and t1 < the
   // end of the track
   double t0;
   double t1;

   double averagePixelsPerSample;
   bool showIndividualSamples;

   sampleCount ssel0;
   sampleCount ssel1;

   wxRect hiddenMid;
   int hiddenLeftOffset;

   wxRect mid;
   int leftOffset;
};

#endif
