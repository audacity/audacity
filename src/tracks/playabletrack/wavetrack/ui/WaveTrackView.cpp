/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveTrackView.h"

#include "../../../../Experimental.h"

#include <wx/graphics.h>

#include "../../../../WaveClip.h"
#include "../../../../WaveTrack.h"

#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../ViewInfo.h"
#include "../../../../prefs/SpectrogramSettings.h"
#include "../../../../prefs/WaveformSettings.h"
#include "../../../../prefs/TracksPrefs.h"

#include "../../../ui/TimeShiftHandle.h"

std::pair<
   bool, // if true, hit-testing is finished
   std::vector<UIHandlePtr>
> WaveTrackSubView::DoDetailedHitTest(
   const TrackPanelMouseState &state,
   const AudacityProject *pProject, int currentTool, bool bMultiTool,
   const std::shared_ptr<WaveTrack> &wt,
   CommonTrackView &view)
{
   return WaveTrackView::DoDetailedHitTest(
      state, pProject, currentTool, bMultiTool, wt, view);
}

WaveTrackView &WaveTrackView::Get( WaveTrack &track )
{
   return static_cast< WaveTrackView& >( TrackView::Get( track ) );
}

const WaveTrackView &WaveTrackView::Get( const WaveTrack &track )
{
   return Get( const_cast<WaveTrack&>( track ) );
}

WaveTrackView::WaveTrackView( const std::shared_ptr<Track> &pTrack )
   : CommonTrackView{ pTrack }
{
   WaveTrackSubViews::BuildAll();

   auto display = TracksPrefs::ViewModeChoice();

   // Force creation always:
   WaveformSettings &settings = static_cast< WaveTrack* >( pTrack.get() )
      ->GetIndependentWaveformSettings();

   if (display == WaveTrackViewConstants::obsoleteWaveformDBDisplay) {
      display = WaveTrackViewConstants::Waveform;
      settings.scaleType = WaveformSettings::stLogarithmic;
   }

   mPlacements.resize( WaveTrackSubViews::size() );

   SetDisplay( display );
}

WaveTrackView::~WaveTrackView()
{
}

void WaveTrackView::CopyTo( Track &track ) const
{
   TrackView::CopyTo( track );
   auto &other = TrackView::Get( track );

   if ( const auto pOther = dynamic_cast< WaveTrackView* >( &other ) ) {
      // only one field is important to preserve in undo/redo history
      pOther->RestorePlacements( SavePlacements() );
   }
}

std::vector<UIHandlePtr> WaveTrackView::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int currentTool, bool bMultiTool)
{
   // should not come here any more, delegation to sub-view instead
   wxASSERT( false );
   return {};
}

std::pair< bool, std::vector<UIHandlePtr> >
WaveTrackView::DoDetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject, int currentTool, bool bMultiTool,
 const std::shared_ptr<WaveTrack> &pTrack,
 CommonTrackView &view)
{
   // common hit-testing for different sub-view types, to help implement their
   // DetailedHitTest()

   // This is the only override of Track::DetailedHitTest that still
   // depends on the state of the Tools toolbar.
   // If that toolbar were eliminated, this could simplify to a sequence of
   // hit test routines describable by a table.

   UIHandlePtr result;
   std::vector<UIHandlePtr> results;

   if (bMultiTool && st.state.CmdDown()) {
      // Ctrl modifier key in multi-tool overrides everything else
      // (But this does not do the time shift constrained to the vertical only,
      //  which is what happens when you hold Ctrl in the Time Shift tool mode)
      result = TimeShiftHandle::HitAnywhere(
         view.mTimeShiftHandle, pTrack, false);
      if (result)
         results.push_back(result);
      return { true, results };
   }
   return { false, results };
}

auto WaveTrackView::GetDisplays() const -> std::vector<WaveTrackDisplay>
{
   // Collect the display types of visible views and sort them by position
   using Pair = std::pair< int, WaveTrackDisplay >;
   std::vector< Pair > pairs;
   size_t ii = 0;
   WaveTrackSubViews::ForEach( [&]( const WaveTrackSubView &subView ){
      auto &placement = mPlacements[ii];
      if ( placement.fraction > 0 )
         pairs.emplace_back( placement.index, subView.SubViewType() );
      ++ii;
   } );
   std::sort( pairs.begin(), pairs.end() );
   std::vector<WaveTrackDisplay> results;
   for ( const auto &pair : pairs )
      results.push_back( pair.second );
   return results;
}

void WaveTrackView::SetDisplay(WaveTrackDisplay display)
{
   size_t ii = 0;
   WaveTrackSubViews::ForEach( [&,display]( WaveTrackSubView &subView ){
      if ( subView.SubViewType() == display )
         mPlacements[ii] = {  0, 1.0 };
      else
         mPlacements[ii] = { -1, 0.0 };
      ++ii;
   } );
}

auto WaveTrackView::GetSubViews( const wxRect &rect ) -> Refinement
{
   Refinement results;

   // Collect the visible views in the right sequence
   using Pair = std::pair< float*, std::shared_ptr< TrackView > >;
   std::vector< Pair > pairs( mPlacements.size() );
   size_t ii = 0;
   float total = 0;
   WaveTrackSubViews::ForEach( [&]( WaveTrackSubView &subView ){
      auto &placement = mPlacements[ii];
      auto index = placement.index;
      auto &fraction = placement.fraction;
      if ( index >= 0 && fraction > 0.0 )
         total += fraction,
         pairs[ index ] = { &fraction, subView.shared_from_this() };
      ++ii;
   } );

   // Remove views we don't need
   auto begin = pairs.begin(), end = pairs.end(),
     newEnd = std::remove_if( begin, end,
        []( const Pair &item ){ return !item.second; } );
   pairs.erase( newEnd, end );
   results.reserve( pairs.size() );

   // Assign coordinates
   // Also update the stored placements, redenominating to the total height,
   // storing integer values
   const auto top = rect.GetTop();
   const auto height = rect.GetHeight();
   float partial = 0;
   wxCoord lastCoord = 0;
   float *lastFraction = nullptr;
   for ( const auto &pair : pairs ) {
      wxCoord newCoord = top + (partial / total) * height;
      results.emplace_back( newCoord, pair.second );
      partial += *pair.first;
      if (lastFraction)
        *lastFraction = newCoord - lastCoord;
      lastFraction = pair.first;
      lastCoord = newCoord;
   }
   if ( lastFraction )
      *lastFraction = top + height - lastCoord;

   return results;
}

std::vector< std::shared_ptr< WaveTrackSubView > >
WaveTrackView::GetAllSubViews()
{
   std::vector< std::shared_ptr< WaveTrackSubView > > results;
   WaveTrackSubViews::ForEach( [&]( WaveTrackSubView &subView ){
      results.push_back( std::static_pointer_cast<WaveTrackSubView>(
         subView.shared_from_this() ) );
   } );
   return results;
}

void WaveTrackView::DoSetMinimized( bool minimized )
{
   // May come here.  Invoke also on sub-views.
   TrackView::DoSetMinimized( minimized );
   WaveTrackSubViews::ForEach( [minimized](WaveTrackSubView &subView){
      subView.DoSetMinimized( minimized );
   } );
}

using DoGetWaveTrackView = DoGetView::Override< WaveTrack >;
template<> template<> auto DoGetWaveTrackView::Implementation() -> Function {
   return [](WaveTrack &track) {
      return std::make_shared<WaveTrackView>( track.SharedPointer() );
   };
}
static DoGetWaveTrackView registerDoGetWaveTrackView;

std::shared_ptr<TrackVRulerControls> WaveTrackView::DoGetVRulerControls()
{
   // This should never be called because of delegation to the spectrum or
   // waveform sub-view
   wxASSERT( false );
   return {};
}

#undef PROFILE_WAVEFORM
#ifdef PROFILE_WAVEFORM
#ifdef __WXMSW__
#include <time.h>
#else
#include <sys/time.h>
#endif
double gWaveformTimeTotal = 0;
int gWaveformTimeCount = 0;

namespace {
   struct Profiler {
      Profiler()
      {
#   ifdef __WXMSW__
         _time64(&tv0);
#   else
         gettimeofday(&tv0, NULL);
#   endif
      }
      
      ~Profiler()
      {
#   ifdef __WXMSW__
         _time64(&tv1);
         double elapsed = _difftime64(tv1, tv0);
#   else
         gettimeofday(&tv1, NULL);
         double elapsed =
         (tv1.tv_sec + tv1.tv_usec*0.000001) -
         (tv0.tv_sec + tv0.tv_usec*0.000001);
#   endif
         gWaveformTimeTotal += elapsed;
         gWaveformTimeCount++;
         wxPrintf(wxT("Avg waveform drawing time: %f\n"),
                  gWaveformTimeTotal / gWaveformTimeCount);
      }
      
#   ifdef __WXMSW__
      __time64_t tv0, tv1;
#else
      struct timeval tv0, tv1;
#endif
   };
}
#endif

ClipParameters::ClipParameters
   (bool spectrum, const WaveTrack *track, const WaveClip *clip, const wxRect &rect,
   const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo)
{
   tOffset = clip->GetOffset();
   rate = clip->GetRate();

   h = zoomInfo.PositionToTime(0, 0
      , true
   );
   h1 = zoomInfo.PositionToTime(rect.width, 0
      , true
   );

   double sel0 = selectedRegion.t0();    //left selection bound
   double sel1 = selectedRegion.t1();    //right selection bound

   //If the track isn't selected, make the selection empty
   if (!track->GetSelected() &&
      (spectrum || !track->IsSyncLockSelected())) { // PRL: why was there a difference for spectrum?
      sel0 = sel1 = 0.0;
   }

   const double trackLen = clip->GetEndTime() - clip->GetStartTime();

   tpre = h - tOffset;                 // offset corrected time of
   //  left edge of display
   tpost = h1 - tOffset;               // offset corrected time of
   //  right edge of display

   const double sps = 1. / rate;            //seconds-per-sample

   // Determine whether we should show individual samples
   // or draw circular points as well
   averagePixelsPerSample = rect.width / (rate * (h1 - h));
   showIndividualSamples = averagePixelsPerSample > 0.5;

   // Calculate actual selection bounds so that t0 > 0 and t1 < the
   // end of the track
   t0 = (tpre >= 0.0 ? tpre : 0.0);
   t1 = (tpost < trackLen - sps * .99 ? tpost : trackLen - sps * .99);
   if (showIndividualSamples) {
      // adjustment so that the last circular point doesn't appear
      // to be hanging off the end
      t1 += 2. / (averagePixelsPerSample * rate);
   }

   // Make sure t1 (the right bound) is greater than 0
   if (t1 < 0.0) {
      t1 = 0.0;
   }

   // Make sure t1 is greater than t0
   if (t0 > t1) {
      t0 = t1;
   }

   // Use the WaveTrack method to show what is selected and 'should' be copied, pasted etc.
   ssel0 = std::max(sampleCount(0), spectrum
      ? sampleCount((sel0 - tOffset) * rate + .99) // PRL: why?
      : track->TimeToLongSamples(sel0 - tOffset)
   );
   ssel1 = std::max(sampleCount(0), spectrum
      ? sampleCount((sel1 - tOffset) * rate + .99) // PRL: why?
      : track->TimeToLongSamples(sel1 - tOffset)
   );

   //trim selection so that it only contains the actual samples
   if (ssel0 != ssel1 && ssel1 > (sampleCount)(0.5 + trackLen * rate)) {
      ssel1 = sampleCount( 0.5 + trackLen * rate );
   }

   // The variable "hiddenMid" will be the rectangle containing the
   // actual waveform, as opposed to any blank area before
   // or after the track, as it would appear without the fisheye.
   hiddenMid = rect;

   // If the left edge of the track is to the right of the left
   // edge of the display, then there's some unused area to the
   // left of the track.  Reduce the "hiddenMid"
   hiddenLeftOffset = 0;
   if (tpre < 0) {
      // Fix Bug #1296 caused by premature conversion to (int).
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset, 0 , true);
      if( time64 < 0 )
         time64 = 0;
      hiddenLeftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      hiddenMid.x += hiddenLeftOffset;
      hiddenMid.width -= hiddenLeftOffset;
   }

   // If the right edge of the track is to the left of the the right
   // edge of the display, then there's some unused area to the right
   // of the track.  Reduce the "hiddenMid" rect by the
   // size of the blank area.
   if (tpost > t1) {
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset+t1, 0 , true);
      if( time64 < 0 )
         time64 = 0;
      const int hiddenRightOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      hiddenMid.width = std::max(0, hiddenRightOffset - hiddenLeftOffset);
   }
   // The variable "mid" will be the rectangle containing the
   // actual waveform, as distorted by the fisheye,
   // as opposed to any blank area before or after the track.
   mid = rect;

   // If the left edge of the track is to the right of the left
   // edge of the display, then there's some unused area to the
   // left of the track.  Reduce the "mid"
   leftOffset = 0;
   if (tpre < 0) {
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset, 0 , false);
      if( time64 < 0 )
         time64 = 0;
      leftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      mid.x += leftOffset;
      mid.width -= leftOffset;
   }

   // If the right edge of the track is to the left of the the right
   // edge of the display, then there's some unused area to the right
   // of the track.  Reduce the "mid" rect by the
   // size of the blank area.
   if (tpost > t1) {
      wxInt64 time64 = zoomInfo.TimeToPosition(tOffset+t1, 0 , false);
      if( time64 < 0 )
         time64 = 0;
      const int distortedRightOffset = (time64 < rect.width) ? (int)time64 : rect.width;

      mid.width = std::max(0, distortedRightOffset - leftOffset);
   }
}

void WaveTrackView::Reparent( const std::shared_ptr<Track> &parent )
{
   CommonTrackView::Reparent( parent );
   WaveTrackSubViews::ForEach( [&parent](WaveTrackSubView &subView){
      subView.Reparent( parent );
   } );
}

void WaveTrackView::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   // Should not come here, drawing is now delegated to sub-views
   wxASSERT( false );

   CommonTrackView::Draw( context, rect, iPass );
}
