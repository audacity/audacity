/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.h

Paul Licameli split from class WaveTrack

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VIEW__
#define __AUDACITY_WAVE_TRACK_VIEW__

#include "../../../ui/CommonTrackView.h"
#include "../../../../ClientData.h"
namespace WaveTrackViewConstants{ enum Display : int; }

class WaveTrack;
class WaveTrackView;

class WaveTrackSubView : public CommonTrackView
{
public:
   explicit
   WaveTrackSubView( WaveTrackView &waveTrackView );
   
   virtual WaveTrackViewConstants::Display SubViewType() const = 0;

   std::pair<
      bool, // if true, hit-testing is finished
      std::vector<UIHandlePtr>
   > DoDetailedHitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject, int currentTool, bool bMultiTool,
      const std::shared_ptr<WaveTrack> &wt,
      CommonTrackView &view);
private:
   std::weak_ptr<WaveTrackView> mwWaveTrackView;
   };

struct WaveTrackSubViewPlacement {
   int index;
   float fraction;
};
using WaveTrackSubViewPlacements = std::vector< WaveTrackSubViewPlacement >;

class WaveTrackView;
using WaveTrackSubViews = ClientData::Site<
   WaveTrackView, WaveTrackSubView, ClientData::SkipCopying, std::shared_ptr
>;

class WaveTrackView final
   : public CommonTrackView
   , public WaveTrackSubViews
{
   WaveTrackView( const WaveTrackView& ) = delete;
   WaveTrackView &operator=( const WaveTrackView& ) = delete;

public:
   static WaveTrackView &Get( WaveTrack &track );
   static const WaveTrackView &Get( const WaveTrack &track );

   explicit
   WaveTrackView( const std::shared_ptr<Track> &pTrack );
   ~WaveTrackView() override;

   // Preserve some view state too for undo/redo purposes
   void CopyTo( Track &track ) const override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;

   // CommonTrackView implementation
   void Reparent( const std::shared_ptr<Track> &parent ) override;

   static std::pair<
      bool, // if true, hit-testing is finished
      std::vector<UIHandlePtr>
   > DoDetailedHitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject, int currentTool, bool bMultiTool,
      const std::shared_ptr<WaveTrack> &wt,
      CommonTrackView &view);

   using WaveTrackDisplay = WaveTrackViewConstants::Display;

   std::vector<WaveTrackDisplay> GetDisplays() const;
   void SetDisplay(WaveTrackDisplay display);

   const WaveTrackSubViewPlacements &SavePlacements() const
      { return mPlacements; }
   void RestorePlacements( const WaveTrackSubViewPlacements &placements )
      { mPlacements = placements; }

   // Get all the sub-views, in a sequence that is unspecified but in
   // correspondence with the result of SavePlacements
   std::vector< std::shared_ptr< WaveTrackSubView > > GetAllSubViews();

private:
   void BuildSubViews() const;
   void DoSetDisplay(WaveTrackDisplay display);

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   // TrackView implementation
   // Get the visible sub-views with top y coordinates
   Refinement GetSubViews( const wxRect &rect ) override;

protected:
   void DoSetMinimized( bool minimized ) override;

   WaveTrackSubViewPlacements mPlacements;
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
