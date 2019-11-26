/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveTrackView.h"

#include "../../../../Experimental.h"

#include <numeric>
#include <wx/graphics.h>

#include "../../../../WaveClip.h"
#include "../../../../WaveTrack.h"

#include "../../../../HitTestResult.h"
#include "../../../../ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../ViewInfo.h"
#include "../../../../prefs/SpectrogramSettings.h"
#include "../../../../prefs/WaveformSettings.h"
#include "../../../../prefs/TracksPrefs.h"

#include "../../../ui/TimeShiftHandle.h"

namespace {

using WaveTrackSubViewPtrs = std::vector< std::shared_ptr< WaveTrackSubView > >;

// Structure that collects and modifies information on sub-view positions
// Written with great generality, allowing any number of sub-views
struct SubViewAdjuster
{
   enum { HotZoneSize = 5 }; // so many pixels at top and bottom of each subview

   SubViewAdjuster( WaveTrackView &view )
      : mwView{
         std::static_pointer_cast<WaveTrackView>( view.shared_from_this() ) }
   {
      mSubViews = view.GetAllSubViews();
      mOrigPlacements = mNewPlacements = view.SavePlacements();
      FindPermutation();
   }

   void FindPermutation()
   {
      // Find a certain sort of the sub-views
      auto size = mOrigPlacements.size();
      wxASSERT( mSubViews.size() == size );
      mPermutation.resize( size );
      const auto begin = mPermutation.begin(), end = mPermutation.end();
      std::iota( begin, end, 0 );
      static auto invisible = []( const WaveTrackSubViewPlacement &placement ){
         return placement.index < 0 || placement.fraction <= 0;
      };
      const auto comp = [this]( size_t ii, size_t jj ){
         auto &pi = mOrigPlacements[ii];
         bool iInvisible = invisible( pi );

         auto &pj = mOrigPlacements[jj];
         bool jInvisible = invisible( pj );

         // Sort the invisibles to the front, rest by index
         if ( iInvisible != jInvisible )
            return iInvisible;
         else if ( !iInvisible )
            return pi.index < pj.index;
         else
            // Minor sort among the invisible views by their type
            return mSubViews[ii]->SubViewType() < mSubViews[jj]->SubViewType();
      };
      std::sort( begin, end, comp );
      // Find the start of visible sub-views
      auto first = std::find_if( begin, end, [this](size_t ii){
         return !invisible( mOrigPlacements[ii] );
      } );
      mFirstSubView = first - begin;
   }

   bool ModifyPermutation( bool top )
   {
      bool rotated = false;
      const auto pBegin = mPermutation.begin(), pEnd = mPermutation.end();
      auto pFirst = pBegin + mFirstSubView;
      if ( mFirstSubView > 0 ) {
         // In case of dragging the top edge of the topmost view, or the
         // bottom edge of the bottommost, decide which of the invisible
         // views can become visible, and reassign the sequence.
         // For definiteness, that choice depends on the subview type numbers;
         // see the sorting criteria above.
         --mFirstSubView;
         --pFirst;
         if ( top ) {
            // If you drag down the top, the greatest-numbered invisible
            // subview type will appear there.
            mNewPlacements[ *pFirst ].fraction = 0;
         }
         else {
            // If you drag up the bottom, let the least-numbered invisible
            // subview type appear there.
            mNewPlacements[ *pBegin ].fraction = 0;
            std::rotate( pBegin, pBegin + 1, pEnd );
            rotated = true;
         }
      }
      // Reassign index numbers to all sub-views and 0 fraction to invisibles
      for ( auto pIter = pBegin; pIter != pFirst; ++pIter ) {
         auto &placement = mNewPlacements[ *pIter ];
         placement.index = -1;
         placement.fraction = 0;
      }
      size_t index = 0;
      for ( auto pIter = pFirst; pIter != pEnd; ++pIter )
         mNewPlacements[ *pIter ].index = index++;
      return rotated;
   }

   std::pair< size_t, bool >
   HitTest( WaveTrackSubView &subView,
      wxCoord yy, wxCoord top, wxCoord height )
   {
      const auto begin = mPermutation.begin(), end = mPermutation.end();
      auto iter = std::find_if( begin, end, [&](size_t ii){
         return mSubViews[ ii ].get() == &subView;
      } );
      auto index = iter - begin;
      auto size = mPermutation.size();
      if ( index < size ) {
         yy -= top;
         if ( yy >= 0 && yy < HotZoneSize && index > 0 )
            return { index, true }; // top hit
         if ( yy < height && yy >= height - HotZoneSize &&
            // Have not yet called ModifyPermutation; dragging bottom of
            // bottommost view allowed only if at least one view is invisible
            ( index < size - 1 || mFirstSubView > 0 ) )
            return { index, false }; // bottom hit
      }
      return { size, false }; // not hit
   }

   void UpdateViews( bool rollback )
   {
      auto pView = mwView.lock();
      if ( pView ) {
         auto pTrack = static_cast< WaveTrack* >( pView->FindTrack().get() );
         for ( auto pChannel : TrackList::Channels<WaveTrack>( pTrack ) )
            WaveTrackView::Get( *pChannel ).RestorePlacements(
               rollback ? mOrigPlacements : mNewPlacements );
      }
   }

   std::weak_ptr< WaveTrackView > mwView;
   WaveTrackSubViewPtrs mSubViews;
   WaveTrackSubViewPlacements mOrigPlacements, mNewPlacements;
   // Array mapping ordinal into the placement and subview arrays
   std::vector< size_t > mPermutation;
   // index into mPermutation
   size_t mFirstSubView{};
};

class SubViewAdjustHandle : public UIHandle
{
public:
   enum { MinHeight = SubViewAdjuster::HotZoneSize };

   static UIHandlePtr HitTest(
      WaveTrackView &view, WaveTrackSubView &subView,
      const TrackPanelMouseState &state )
   {
      SubViewAdjuster adjuster{ view };
      auto hit = adjuster.HitTest( subView,
         state.state.GetY(), state.rect.GetTop(), state.rect.GetHeight() );
      auto index = hit.first;

      if ( index < adjuster.mPermutation.size() )
         return std::make_shared< SubViewAdjustHandle >(
            std::move( adjuster ), index, hit.second
         );
      else
         return {};
   }

   SubViewAdjustHandle(
      SubViewAdjuster &&adjuster, size_t subViewIndex, bool top )
      : mAdjuster{ std::move( adjuster ) }
      , mMySubView{ subViewIndex }
      , mTop{ top }
   {
      if ( mAdjuster.ModifyPermutation( top ) )
         --mMySubView;
   }

   Result Click(
      const TrackPanelMouseEvent &event, AudacityProject * ) override
   {
      using namespace RefreshCode;
      const auto &permutation = mAdjuster.mPermutation;
      const auto size = permutation.size();
      if ( mMySubView >= size )
         return Cancelled;

      const auto &rect = event.rect;
      const auto height = rect.GetHeight();
      mOrigHeight = height;

      wxASSERT( height ==
         mAdjuster.mOrigPlacements[ mAdjuster.mPermutation[ mMySubView ] ]
            .fraction
      );

      // Find the total height of the sub-views that may resize
      // Note that this depends on the redenomination of fractions that
      // happened in the last call to GetSubViews
      mTotalHeight = 0;
      const auto begin = permutation.begin();
      auto iter = begin + ( mTop ? mAdjuster.mFirstSubView : mMySubView );
      const auto end = ( mTop ? begin + mMySubView + 1 : permutation.end() );
      for (; iter != end; ++iter) {
         const auto &placement = mAdjuster.mOrigPlacements[ *iter ];
         mTotalHeight += placement.fraction;
      }

      // Compute the maximum and minimum Y coordinates for drag effect
      if ( mTop ) {
         mOrigY = rect.GetTop();
         mYMax = rect.GetBottom();
         mYMin = mYMax - mTotalHeight + 1;
      }
      else {
         mOrigY = rect.GetBottom();
         mYMin = rect.GetTop();
         mYMax = mYMin + mTotalHeight - 1;
      }

      return RefreshNone;
   }

   Result Drag( const TrackPanelMouseEvent &event, AudacityProject * ) override
   {
      using namespace RefreshCode;
      auto pView = mAdjuster.mwView.lock();
      if ( !pView )
         return Cancelled;

      // Find new height for the dragged sub-view
      auto newY = std::max( mYMin, std::min( mYMax, event.event.GetY() ) );
      const auto delta = newY - mOrigY;
      wxCoord newHeight = mTop
         ? mOrigHeight - delta
         : mOrigHeight + delta
      ;
      wxASSERT( newHeight >= 0 && newHeight <= mTotalHeight );
      if ( newHeight < MinHeight )
         // Snap the dragged sub-view to nothing
         newHeight = 0;

      // Reassign height for the dragged sub-view
      auto &myPlacement =
         mAdjuster.mNewPlacements[ mAdjuster.mPermutation[ mMySubView ] ];
      myPlacement.fraction = newHeight;

      // Grow or shrink other sub-views
      auto excess = newHeight - mOrigHeight; // maybe negative
      const auto adjustHeight = [&](size_t ii) {
         if (excess == 0)
            return true;

         auto index = mAdjuster.mPermutation[ ii ];

         const auto &origPlacement = mAdjuster.mOrigPlacements[ index ];
         const auto oldFraction = origPlacement.fraction;

         auto &placement = mAdjuster.mNewPlacements[ index ];
         auto &fraction = placement.fraction;

         if (excess > oldFraction) {
            excess -= oldFraction, fraction = 0;
            return false;
         }
         else {
            auto newFraction = oldFraction - excess;
            if ( newFraction < MinHeight ) {
               // This snaps very short sub-views to nothing
               myPlacement.fraction += newFraction;
               fraction = 0;
            }
            else
               fraction = newFraction;
            return true;
         }
      };
      if ( mTop ) {
         for ( size_t ii = mMySubView; ii > 0; ) {
            --ii;
            if ( adjustHeight( ii ) )
               break;
         }
      }
      else {
         for ( size_t ii = mMySubView + 1, size = mAdjuster.mPermutation.size();
            ii < size; ++ii
         ) {
            if ( adjustHeight( ii ) )
               break;
         }
      }

      // Save adjustment to the track and request a redraw
      mAdjuster.UpdateViews( false );
      return RefreshAll;
   }

   HitTestPreview Preview(
      const TrackPanelMouseState &state, const AudacityProject * ) override
   {
      static wxCursor resizeCursor{ wxCURSOR_SIZENS };
      return {
         _("Click and drag to adjust sizes of sub-views."),
         &resizeCursor
      };
   }

   Result Release(
      const TrackPanelMouseEvent &event, AudacityProject *pProject,
      wxWindow *pParent) override
   {
      ProjectHistory::Get( *pProject ).ModifyState( false );
      return RefreshCode::RefreshNone;
   }

   Result Cancel( AudacityProject * ) override
   {
      mAdjuster.UpdateViews( true );
      return RefreshCode::RefreshAll;
   }

private:

   SubViewAdjuster mAdjuster;

   // An index into mAdjuster.mPermutation
   size_t mMySubView{};

   wxCoord mYMin{}, mYMax{};
   wxCoord mTotalHeight{};
   wxCoord mOrigHeight{};
   wxCoord mOrigY{};

   // Whether we drag the top or the bottom of the sub-view
   bool mTop{};
};

}

std::pair<
   bool, // if true, hit-testing is finished
   std::vector<UIHandlePtr>
> WaveTrackSubView::DoDetailedHitTest(
   const TrackPanelMouseState &state,
   const AudacityProject *pProject, int currentTool, bool bMultiTool,
   const std::shared_ptr<WaveTrack> &wt,
   CommonTrackView &view)
{
   auto results = WaveTrackView::DoDetailedHitTest(
      state, pProject, currentTool, bMultiTool, wt, view );
   if ( results.first )
      return results;

   auto pWaveTrackView = mwWaveTrackView.lock();
   if ( pWaveTrackView && !state.state.HasModifiers() ) {
      auto pHandle = SubViewAdjustHandle::HitTest(
         *pWaveTrackView, *this, state );
      if (pHandle)
         results.second.push_back( pHandle );
   }

   return results;
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
}

WaveTrackSubView::WaveTrackSubView( WaveTrackView &waveTrackView )
   : CommonTrackView( waveTrackView.FindTrack() )
{
   mwWaveTrackView = std::static_pointer_cast<WaveTrackView>(
      waveTrackView.shared_from_this() );
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
   BuildSubViews();

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
   BuildSubViews();
   DoSetDisplay( display );
}

void WaveTrackView::DoSetDisplay(WaveTrackDisplay display)
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
   BuildSubViews();

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
   BuildSubViews();

   std::vector< std::shared_ptr< WaveTrackSubView > > results;
   WaveTrackSubViews::ForEach( [&]( WaveTrackSubView &subView ){
      results.push_back( std::static_pointer_cast<WaveTrackSubView>(
         subView.shared_from_this() ) );
   } );
   return results;
}

void WaveTrackView::DoSetMinimized( bool minimized )
{
   BuildSubViews();

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
   // BuildSubViews(); // not really needed
   CommonTrackView::Reparent( parent );
   WaveTrackSubViews::ForEach( [&parent](WaveTrackSubView &subView){
      subView.Reparent( parent );
   } );
}

void WaveTrackView::BuildSubViews() const
{
   if ( WaveTrackSubViews::size() == 0) {
      // On-demand steps that can't happen in the constructor
      auto pThis = const_cast<WaveTrackView*>( this );
      pThis->BuildAll();
      bool minimized = GetMinimized();
      pThis->WaveTrackSubViews::ForEach( [&]( WaveTrackSubView &subView ){
         subView.DoSetMinimized( minimized );
      } );

      if ( pThis->mPlacements.empty() ) {
         pThis->mPlacements.resize( WaveTrackSubViews::size() );
         
         auto pTrack = pThis->FindTrack();
         auto display = TracksPrefs::ViewModeChoice();
         
         // Force creation always:
         WaveformSettings &settings = static_cast< WaveTrack* >( pTrack.get() )
         ->GetIndependentWaveformSettings();
         
         if (display == WaveTrackViewConstants::obsoleteWaveformDBDisplay) {
            display = WaveTrackViewConstants::Waveform;
            settings.scaleType = WaveformSettings::stLogarithmic;
         }
         
         pThis->DoSetDisplay( display );
      }
   }
}

void WaveTrackView::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   // Should not come here, drawing is now delegated to sub-views
   wxASSERT( false );

   CommonTrackView::Draw( context, rect, iPass );
}
