/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.h

Paul Licameli split from class WaveTrack

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VIEW__
#define __AUDACITY_WAVE_TRACK_VIEW__

#include "../../../ui/CommonTrackView.h"
#include "../../../../ClientData.h"
#include "SampleCount.h"
namespace WaveTrackViewConstants{ enum Display : int; }
struct WaveTrackSubViewType;

class CutlineHandle;
class TranslatableString;
class WaveTrack;
class WaveTrackView;
class WaveClip;

class wxDC;

class AUDACITY_DLL_API WaveTrackSubView : public CommonTrackView
{
public:

   using Display = WaveTrackViewConstants::Display;
   using Type = WaveTrackSubViewType;

   explicit
   WaveTrackSubView( WaveTrackView &waveTrackView );
   
   virtual const Type &SubViewType() const = 0;

   std::pair<
      bool, // if true, hit-testing is finished
      std::vector<UIHandlePtr>
   > DoDetailedHitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject, int currentTool, bool bMultiTool,
      const std::shared_ptr<WaveTrack> &wt );
   
protected:
   static void DrawBoldBoundaries(
      TrackPanelDrawingContext &context, const WaveTrack *track,
      const wxRect &rect );

   std::weak_ptr<WaveTrackView> GetWaveTrackView() const;

private:
   std::weak_ptr<UIHandle> mCloseHandle;
   std::weak_ptr<UIHandle> mAdjustHandle;
   std::weak_ptr<UIHandle> mRearrangeHandle;
   std::weak_ptr<CutlineHandle> mCutlineHandle;
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

class AUDACITY_DLL_API WaveTrackView final
   : public CommonTrackView
   , public WaveTrackSubViews
{
   WaveTrackView( const WaveTrackView& ) = delete;
   WaveTrackView &operator=( const WaveTrackView& ) = delete;

public:
   using Display = WaveTrackViewConstants::Display;

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

   std::vector< WaveTrackSubView::Type > GetDisplays() const;
   void SetDisplay(Display display, bool exclusive = true);

   const WaveTrackSubViewPlacements &SavePlacements() const
      { return mPlacements; }
   void RestorePlacements( const WaveTrackSubViewPlacements &placements )
      { mPlacements = placements; }

   // Return true if successful.  Fails if you try to toggle off the only
   // sub-view.
   bool ToggleSubView( Display id );

   // Get all the sub-views, in a sequence that is unspecified but in
   // correspondence with the result of SavePlacements
   std::vector< std::shared_ptr< WaveTrackSubView > > GetAllSubViews();

   // Return cached height of rect in last call of GetSubViews
   wxCoord GetLastHeight() const { return mLastHeight; }

   bool GetMultiView() const { return mMultiView; }
   void SetMultiView( bool value ) { mMultiView = value; }


   std::weak_ptr<WaveClip> GetSelectedClip();

private:
   void BuildSubViews() const;
   void DoSetDisplay(Display display, bool exclusive = true);

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
   std::shared_ptr<CommonTrackCell> DoGetAffordanceControls() override;

   void DoSetMinimized( bool minimized ) override;

   // Placements are in correspondence with the array of sub-views
   // in the WaveTrackSubViews base class, though their sequence is
   // unspecified and maybe different in different platforms.
   WaveTrackSubViewPlacements mPlacements;
   mutable wxCoord mLastHeight{};

   bool mMultiView{ false };
};

// Helper for drawing routines
class SelectedRegion;
class WaveClip;
class ZoomInfo;

struct AUDACITY_DLL_API ClipParameters
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

   // returns a clip rectangle restricted by viewRect, 
   // and with clipOffsetX - clip horizontal origin offset within view rect
   static wxRect GetClipRect(const WaveClip& clip, const ZoomInfo& zoomInfo, const wxRect& viewRect, int clipOffsetX = 0);
};

#endif
