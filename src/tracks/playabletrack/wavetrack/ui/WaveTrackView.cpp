/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveTrackView.h"

#include <unordered_set>

#include "CutlineHandle.h"

#include <numeric>
#include <wx/dc.h>
#include <wx/graphics.h>

#include "../../../../AColor.h"
#include "../../../../WaveClip.h"
#include "../../../../WaveTrack.h"

#include "../../../../../images/Cursors.h"
#include "../../../../AllThemeResources.h"

#include "../../../../HitTestResult.h"
#include "../../../../ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../ViewInfo.h"
#include "../../../../prefs/TracksPrefs.h"

#include "../../../ui/TimeShiftHandle.h"
#include "../../../ui/ButtonHandle.h"
#include "../../../../TrackInfo.h"

#include "WaveTrackAffordanceControls.h"

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

   size_t NVisible() const
   { return mPermutation.size() - mFirstSubView; }

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

   size_t FindIndex( WaveTrackSubView &subView ) const
   {
      const auto begin = mPermutation.begin(), end = mPermutation.end();
      auto iter = std::find_if( begin, end, [&](size_t ii){
         return mSubViews[ ii ].get() == &subView;
      } );
      return iter - begin;
   }

   std::pair< size_t, bool >
   HitTest( WaveTrackSubView &subView,
      wxCoord yy, wxCoord top, wxCoord height )
   {
      const auto index = FindIndex( subView );
      auto size = mPermutation.size();
      if ( index < (int)size ) {
         yy -= top;
         if ( yy >= 0 && yy < HotZoneSize && index > 0 )
            return { index, true }; // top hit
         if ( yy < height && yy >= height - HotZoneSize &&
            // Have not yet called ModifyPermutation; dragging bottom of
            // bottommost view allowed only if at least one view is invisible
            ( index < (int)size - 1 || mFirstSubView > 0 ) )
            return { index, false }; // bottom hit
      }
      return { size, false }; // not hit
   }

   std::vector<wxCoord> ComputeHeights( wxCoord totalHeight )
   {
      // Compute integer-valued heights
      float total = 0;
      for (const auto index : mPermutation ) {
         const auto &placement = mOrigPlacements[ index ];
         total += std::max( 0.f, placement.fraction );
      }
      float partial = 0;
      wxCoord lastCoord = 0;
      std::vector<wxCoord> result;
      for (const auto index : mPermutation ) {
         const auto &placement = mOrigPlacements[ index ];
         auto fraction = std::max( 0.f, placement.fraction );
         wxCoord coord = ( (partial + fraction ) / total ) * totalHeight;
         auto height = coord - lastCoord;
         result.emplace_back( height );
         mNewPlacements[ index ].fraction = height;
         lastCoord = coord;
         partial += fraction;
      }
      return result;
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

   static UIHandlePtr HitTest( std::weak_ptr<UIHandle> &holder,
      WaveTrackView &view,
      WaveTrackSubView &subView,
      const TrackPanelMouseState &state )
   {
      if ( !view.GetMultiView() )
         return {};

      SubViewAdjuster adjuster{ view };
      auto hit = adjuster.HitTest( subView,
         state.state.GetY(), state.rect.GetTop(), state.rect.GetHeight() );
      auto index = hit.first;

      if ( index < adjuster.mPermutation.size() ) {
         UIHandlePtr result = std::make_shared< SubViewAdjustHandle >(
            std::move( adjuster ), index, view.GetLastHeight(), hit.second
         );
         result = AssignUIHandlePtr( holder, result );
         return result;
      }
      else
         return {};
   }

   SubViewAdjustHandle(
      SubViewAdjuster &&adjuster, size_t subViewIndex,
         wxCoord viewHeight, bool top )
      : mAdjuster{ std::move( adjuster ) }
      , mMySubView{ subViewIndex }
      , mViewHeight{ viewHeight }
      , mTop{ top }
   {
      if ( mAdjuster.ModifyPermutation( top ) )
         --mMySubView;
   }

   Result Click(
      const TrackPanelMouseEvent &event, AudacityProject *pProject ) override
   {
      using namespace RefreshCode;
      const auto &permutation = mAdjuster.mPermutation;
      const auto size = permutation.size();
      if ( mMySubView >= size )
         return Cancelled;

      if (event.event.LeftDClick()) {
         for ( auto &placement : mAdjuster.mNewPlacements ) {
            if ( placement.index >= 0 )
               placement.fraction = 1.0f;
            else
               placement.fraction = 0.0f;
         }
         mAdjuster.UpdateViews( false );
         ProjectHistory::Get( *pProject ).ModifyState( false );

         // Do not start a drag
         return Cancelled | RefreshAll;
      }

      const auto &rect = event.rect;
      const auto height = rect.GetHeight();
      mOrigHeight = height;

      mOrigHeights = mAdjuster.ComputeHeights( mViewHeight );
      
      // Find the total height of the sub-views that may resize
      mTotalHeight = 0;
      auto index = ( mTop ? mAdjuster.mFirstSubView : mMySubView );
      const auto end = ( mTop ?  mMySubView + 1 : permutation.size() );
      for (; index != end; ++index)
         mTotalHeight += mOrigHeights[ index ];

      wxASSERT( height == mOrigHeights[ mMySubView ] );

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

         const auto oldFraction = mOrigHeights[ ii ];

         auto index = mAdjuster.mPermutation[ ii ];
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
      const TrackPanelMouseState &state, AudacityProject * ) override
   {
      static auto resizeCursor =
         ::MakeCursor(wxCURSOR_ARROW, SubViewsCursorXpm, 16, 16);
      return {
         XO(
"Click and drag to adjust sizes of sub-views, double-click to split evenly"),
         &*resizeCursor
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
   std::vector<wxCoord> mOrigHeights;

   // An index into mAdjuster.mPermutation
   size_t mMySubView{};

   wxCoord mYMin{}, mYMax{};
   wxCoord mViewHeight{}; // Total height of all sub-views
   wxCoord mTotalHeight{}; // Total height of adjusting sub-views only
   wxCoord mOrigHeight{};
   wxCoord mOrigY{};

   // Whether we drag the top or the bottom of the sub-view
   bool mTop{};
};

class SubViewRearrangeHandle : public UIHandle
{
public:
   // Make it somewhat wider than the close button
   enum { HotZoneWidth = 3 * kTrackInfoBtnSize / 2 };

   static UIHandlePtr HitTest(  std::weak_ptr<UIHandle> &holder,
      WaveTrackView &view, WaveTrackSubView &subView,
      const TrackPanelMouseState &state )
   {
      if ( !view.GetMultiView() )
         return {};

      SubViewAdjuster adjuster{ view };
      if ( adjuster.NVisible() < 2 )
         return {};

      auto relX = state.state.GetX() - state.rect.GetLeft();
      if ( relX >= HotZoneWidth )
         return {};

      auto index = adjuster.FindIndex( subView );

      // Hit on the rearrange cursor only in the top and bottom thirds of
      // sub-view height, leaving the rest free to hit the selection cursor
      // first.
      // And also exclude the top third of the topmost sub-view and bottom
      // third of bottommost.
      auto relY = state.state.GetY() - state.rect.GetTop();
      auto height = state.rect.GetHeight();
      bool hit =
         ( ( 3 * relY < height ) && index > 0 ) // top hit
      ||
         ( ( 3 * relY > 2 * height ) &&
           index < adjuster.mPermutation.size() - 1 ) // bottom
      ;
      if ( ! hit )
         return {};

      UIHandlePtr result = std::make_shared< SubViewRearrangeHandle >(
         std::move( adjuster ),
         index, view.GetLastHeight()
      );
      result = AssignUIHandlePtr( holder, result );
      return result;
   }

   SubViewRearrangeHandle(
      SubViewAdjuster &&adjuster, size_t subViewIndex,
         wxCoord viewHeight )
      : mAdjuster{ std::move( adjuster ) }
      , mMySubView{ subViewIndex }
      , mViewHeight{ viewHeight }
   {
   }
   
   Result Click(
      const TrackPanelMouseEvent &event, AudacityProject *pProject ) override
   {
      using namespace RefreshCode;
      const auto &permutation = mAdjuster.mPermutation;
      const auto size = permutation.size();
      if ( mMySubView >= size )
         return Cancelled;

      mHeights = mAdjuster.ComputeHeights( mViewHeight );

      // Find y coordinate of first sub-view
      wxCoord heightAbove = 0;
      for (auto index = mAdjuster.mFirstSubView;
         index != mMySubView; ++index)
         heightAbove += mHeights[ index ];
      mTopY = event.rect.GetTop() - heightAbove;

      return RefreshNone;
   }

   bool Clicked() const { return !mHeights.empty(); }

   enum DragChoice_t{ Upward, Downward, Neutral };

   DragChoice_t DragChoice( const TrackPanelMouseEvent &event ) const
   {
      // Disregard x coordinate -- so the mouse need not be in any sub-view,
      // just in the correct range of y coordinates
      auto yy = event.event.GetY();
      auto coord = mTopY;
      size_t ii = mAdjuster.mFirstSubView;
      if ( yy < mTopY )
         return ( mMySubView == ii ) ? Neutral : Upward;

      for ( auto nn = mHeights.size(); ii < nn; ++ii ) {
         const auto height = mHeights[ ii ];
         coord += height;
         if ( yy < coord )
            break;
      }

      if ( ii < mMySubView ) {
         if ( yy < coord - mHeights[ ii ] + mHeights[ mMySubView ] )
            return Upward;
      }
   
      if ( ii > mMySubView ) {
         if( mMySubView < mHeights.size() - 1 &&
            yy >= coord - mHeights[ mMySubView ] )
         return Downward;
      }
   
      return Neutral;
   }

   Result Drag( const TrackPanelMouseEvent &event, AudacityProject * ) override
   {
      using namespace RefreshCode;
      auto pView = mAdjuster.mwView.lock();
      if ( !pView )
         return Cancelled;

      switch( DragChoice( event ) ) {
         case Upward:
         {
            std::swap( mHeights[ mMySubView ], mHeights[ mMySubView - 1 ] );
            std::swap(
               mAdjuster.mNewPlacements[ mMySubView ].index,
               mAdjuster.mNewPlacements[ mMySubView - 1 ].index
            );
            --mMySubView;
            break;
         }
         case Downward:
         {
            std::swap( mHeights[ mMySubView ], mHeights[ mMySubView + 1 ] );
            std::swap(
               mAdjuster.mNewPlacements[ mMySubView ].index,
               mAdjuster.mNewPlacements[ mMySubView + 1 ].index
            );
            ++mMySubView;
            break;
         }
         default:
            return RefreshNone;
      }

      // Save adjustment to the track and request a redraw
      mAdjuster.UpdateViews( false );
      return RefreshAll;
   }

   HitTestPreview Preview(
      const TrackPanelMouseState &state, AudacityProject * ) override
   {
      static auto hoverCursor =
         ::MakeCursor(wxCURSOR_HAND, RearrangeCursorXpm, 16, 16);
      static auto clickedCursor =
         ::MakeCursor(wxCURSOR_HAND, RearrangingCursorXpm, 16, 16);
      return {
         XO("Click and drag to rearrange sub-views"),
         Clicked() ? &*clickedCursor : &*hoverCursor,
         XO("Rearrange sub-views")
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
   std::vector<wxCoord> mHeights;
   wxCoord mTopY;

   // An index into mAdjuster.mPermutation
   size_t mMySubView{};

   wxCoord mViewHeight{}; // Total height of all sub-views
};

class SubViewCloseHandle : public ButtonHandle
{
   static wxRect GetButtonRect( const wxRect &rect )
   {
      return {
         rect.GetLeft(),
         rect.GetTop(),
         kTrackInfoBtnSize,
         kTrackInfoBtnSize
      };
   }

public:
   static UIHandlePtr HitTest( std::weak_ptr<UIHandle> &holder,
      WaveTrackView &view, WaveTrackSubView &subView,
      const TrackPanelMouseState &state )
   {
      SubViewAdjuster adjuster{ view };
      if ( adjuster.NVisible() < 2 )
         return {};
   
      const auto rect = GetButtonRect( state.rect );
      if ( !rect.Contains( state.state.GetPosition() ) )
         return {};
      auto index = adjuster.FindIndex( subView );
      UIHandlePtr result = std::make_shared<SubViewCloseHandle>(
         std::move( adjuster ), index, view.FindTrack(), rect );
      result = AssignUIHandlePtr( holder, result );
      return result;
   }

   SubViewCloseHandle(
      SubViewAdjuster &&adjuster, size_t index,
      const std::shared_ptr<Track> &pTrack, const wxRect &rect )
      : ButtonHandle{ pTrack, rect }
      , mAdjuster{ std::move( adjuster ) }
      , mMySubView{ index }
   {
   }

   Result CommitChanges(
      const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
      override
   {
      ProjectHistory::Get( *pProject ).ModifyState( false );
      auto &myPlacement =
         mAdjuster.mNewPlacements[ mAdjuster.mPermutation[ mMySubView ] ];
      myPlacement.fraction = 0;
      mAdjuster.UpdateViews( false );
      return RefreshCode::RefreshAll;
   }

   TranslatableString Tip(
      const wxMouseState &state, AudacityProject &project) const override
   {
      return XO("Close sub-view");
   }

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context, const wxRect &rect, unsigned iPass )
      override
   {
      if ( iPass == TrackArtist::PassMargins ) { // after PassTracks
          TrackInfo::DrawCloseButton(
             context, GetButtonRect(rect), GetTrack().get(), this );
      }
   }

private:
   SubViewAdjuster mAdjuster;
   size_t mMySubView{};
};

}

std::pair<
   bool, // if true, hit-testing is finished
   std::vector<UIHandlePtr>
> WaveTrackSubView::DoDetailedHitTest(
   const TrackPanelMouseState &state,
   const AudacityProject *pProject, int currentTool, bool bMultiTool,
   const std::shared_ptr<WaveTrack> &wt)
{
   auto results = WaveTrackView::DoDetailedHitTest(
      state, pProject, currentTool, bMultiTool, wt, *this );
   if ( results.first )
      return results;

   auto pWaveTrackView = mwWaveTrackView.lock();
   if ( pWaveTrackView && !state.state.HasModifiers() ) {
      if ( auto pHandle = SubViewCloseHandle::HitTest(
         mCloseHandle,
         *pWaveTrackView, *this, state ) )
         results.second.push_back( pHandle );
      if ( auto pHandle = SubViewAdjustHandle::HitTest(
         mAdjustHandle,
         *pWaveTrackView, *this, state ) )
         results.second.push_back( pHandle );
      if ( auto pHandle = SubViewRearrangeHandle::HitTest(
         mRearrangeHandle,
         *pWaveTrackView, *this, state ) )
         results.second.push_back( pHandle );
   }
   if (auto result = CutlineHandle::HitTest(
      mCutlineHandle, state.state, state.rect,
      pProject, wt ))
      // This overriding test applies in all tools
      results.second.push_back(result);

   return results;
}


void WaveTrackSubView::DrawBoldBoundaries(
   TrackPanelDrawingContext &context, const WaveTrack *track,
   const wxRect &rect )
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );

   const auto &zoomInfo = *artist->pZoomInfo;

#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   auto target2 = dynamic_cast<CutlineHandle*>(context.target.get());
#endif
   for (const auto loc : track->GetCachedLocations()) {
      bool highlightLoc = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
      highlightLoc =
         target2 && target2->GetTrack().get() == track &&
         target2->GetLocation() == loc;
#endif
      const int xx = zoomInfo.TimeToPosition(loc.pos);
      if (xx >= 0 && xx < rect.width) {
         dc.SetPen( highlightLoc ? AColor::uglyPen : *wxGREY_PEN );
         AColor::Line(dc, (int) (rect.x + xx - 1), rect.y, (int) (rect.x + xx - 1), rect.y + rect.height);
         if (loc.typ == WaveTrackLocation::locationCutLine) {
            dc.SetPen( highlightLoc ? AColor::uglyPen : *wxRED_PEN );
         }
         else {
#ifdef EXPERIMENTAL_DA
            // JKC Black does not show up enough.
            dc.SetPen(highlightLoc ? AColor::uglyPen : *wxWHITE_PEN);
#else
            dc.SetPen(highlightLoc ? AColor::uglyPen : *wxBLACK_PEN);
#endif
         }
         AColor::Line(dc, (int) (rect.x + xx), rect.y, (int) (rect.x + xx), rect.y + rect.height);
         dc.SetPen( highlightLoc ? AColor::uglyPen : *wxGREY_PEN );
         AColor::Line(dc, (int) (rect.x + xx + 1), rect.y, (int) (rect.x + xx + 1), rect.y + rect.height);
      }
   }
}

std::weak_ptr<WaveTrackView> WaveTrackSubView::GetWaveTrackView() const
{
   return mwWaveTrackView;
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
      // only these fields are important to preserve in undo/redo history
      pOther->RestorePlacements( SavePlacements() );
      pOther->mMultiView = mMultiView;
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

auto WaveTrackView::GetDisplays() const
   -> std::vector< WaveTrackSubView::Type >
{
   BuildSubViews();

   // Collect the display types of visible views and sort them by position
   using Pair = std::pair< int, WaveTrackSubView::Type >;
   std::vector< Pair > pairs;
   size_t ii = 0;
   WaveTrackSubViews::ForEach( [&]( const WaveTrackSubView &subView ){
      auto &placement = mPlacements[ii];
      if ( placement.fraction > 0 )
         pairs.emplace_back( placement.index, subView.SubViewType() );
      ++ii;
   } );
   std::sort( pairs.begin(), pairs.end() );
   std::vector< WaveTrackSubView::Type > results;
   for ( const auto &pair : pairs )
      results.push_back( pair.second );
   return results;
}

void WaveTrackView::SetDisplay(Display display, bool exclusive)
{
   BuildSubViews();
   DoSetDisplay( display, exclusive );
}

bool WaveTrackView::ToggleSubView(Display display)
{
   size_t ii = 0;
   size_t found = 0;
   if ( WaveTrackSubViews::FindIf( [&]( const WaveTrackSubView &subView ) {
      if ( subView.SubViewType().id == display ) {
         found = ii;
         return true;
      }
      ++ii;
      return false;
   } ) ) {
      auto &foundPlacement = mPlacements[found];
      if ( foundPlacement.fraction > 0.0 ) {
         // Toggle off

         if (GetDisplays().size() < 2)
            // refuse to do it
            return false;
         
         auto index = foundPlacement.index;
         foundPlacement = { -1, 0.0 };
         if (index >= 0) {
            for ( auto &placement : mPlacements ) {
               if ( placement.index > index )
                  --placement.index;
            }
         }

         return true;
      }
      else {
         // Toggle on
         float total = 0;
         int greatest = -1;
         unsigned nn = 0;
         for ( const auto &placement : mPlacements ) {
            if ( placement.fraction > 0.0 && placement.index >= 0 ) {
               total += placement.fraction;
               greatest = std::max( greatest, placement.index );
               ++nn;
            }
         }
         // Turn on the sub-view, putting it lowest, and with average of the
         // heights of the other sub-views
         foundPlacement = { greatest + 1, total / nn };

         return true;
      }
   }
   else
      // unknown sub-view
      return false;
}

// If exclusive, make the chosen view take up all the height.  Else,
// partition equally, putting the specified view on top.
// Be sure the sequence in which the other views appear is determinate.
void WaveTrackView::DoSetDisplay(Display display, bool exclusive)
{
   // Some generality here anticipating more than two views.
   // The order of sub-views in the array is not specified, so make it definite
   // by sorting by the view type constants.
   size_t ii = 0;
   std::vector< std::pair< WaveTrackViewConstants::Display, size_t > > pairs;
   WaveTrackSubViews::ForEach( [&pairs, &ii]( WaveTrackSubView &subView ){
      pairs.push_back( { subView.SubViewType().id, ii++ } );
   } );
   std::sort( pairs.begin(), pairs.end() );

   int jj = 1;
   for ( const auto &pair : pairs ) {
      auto &placement = mPlacements[ pair.second ];
      if (pair.first == display) {
         // 0 for first view
         placement = { 0, 1.0 };
      }
      else if( exclusive )
         // -1 for not displayed
         placement = { -1, 0.0 };
      else
         // positions other than the first.
         // (Note that the fractions in the placement don't need to be
         // denominated to 1.  Just make them all equal to get an equal
         // partitioning of the sub-views.)
         placement = { jj++, 1.0 };
   }
}

auto WaveTrackView::GetSubViews( const wxRect &rect ) -> Refinement
{
   BuildSubViews();

   // Collect the visible views in the right sequence
   struct Item {
      int index; float fraction; std::shared_ptr< TrackView > pView;
   };
   std::vector< Item > items;
   size_t ii = 0;
   float total = 0;
   WaveTrackSubViews::ForEach( [&]( WaveTrackSubView &subView ){
      auto &placement = mPlacements[ii];
      auto index = placement.index;
      auto fraction = placement.fraction;
      if ( index >= 0 && fraction > 0.0 )
         total += fraction,
         items.push_back( { index, fraction, subView.shared_from_this() } );
      ++ii;
   } );
   std::sort( items.begin(), items.end(), [](const Item &a, const Item &b){
      return a.index < b.index;
   } );

   // Remove views we don't need
   auto begin = items.begin(), end = items.end(),
     newEnd = std::remove_if( begin, end,
        []( const Item &item ){ return !item.pView; } );
   items.erase( newEnd, end );

   // Assign coordinates, redenominating to the total height,
   // storing integer values
   Refinement results;
   results.reserve( items.size() );
   const auto top = rect.GetTop();
   const auto height = rect.GetHeight();
   float partial = 0;
   wxCoord lastCoord = 0;
   for ( const auto &item : items ) {
      wxCoord newCoord = top + (partial / total) * height;
      results.emplace_back( newCoord, item.pView );
      partial += item.fraction;
   }

   // Cache for the use of sub-view dragging
   mLastHeight = height;

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

std::shared_ptr<CommonTrackCell> WaveTrackView::DoGetAffordanceControls()
{
   return std::make_shared<WaveTrackAffordanceControls>(FindTrack());
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

   // If the right edge of the track is to the left of the right
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

   // If the right edge of the track is to the left of the right
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

wxRect ClipParameters::GetClipRect(const WaveClip& clip, const ZoomInfo& zoomInfo, const wxRect& viewRect, int clipOffsetX)
{
    auto srs = 1. / static_cast<double>(clip.GetRate());
    //to prevent overlap left and right most samples with frame border
    auto margin = .25 * srs;
    auto edgeLeft = static_cast<wxInt64>(viewRect.GetLeft());
    auto edgeRight = static_cast<wxInt64>(viewRect.GetRight());
    auto left = std::clamp(zoomInfo.TimeToPosition(clip.GetOffset() - margin, viewRect.x + clipOffsetX, true), edgeLeft, edgeRight);
    auto right = std::clamp(zoomInfo.TimeToPosition(clip.GetEndTime() - srs + margin, viewRect.x + clipOffsetX, true), edgeLeft, edgeRight);
    if (right - left > 0)
    {
        //after clamping we can expect that left and right 
        //are small enough to be put into int
        return wxRect(static_cast<int>(left), viewRect.y, static_cast<int>(right - left), viewRect.height);
    }
    //off the screen
    return wxRect();
}

void WaveTrackView::Reparent( const std::shared_ptr<Track> &parent )
{
   // BuildSubViews(); // not really needed
   CommonTrackView::Reparent( parent );
   WaveTrackSubViews::ForEach( [&parent](WaveTrackSubView &subView){
      subView.Reparent( parent );
   } );
   if (mpAffordanceCellControl)
      mpAffordanceCellControl->Reparent(parent);
}

std::weak_ptr<WaveClip> WaveTrackView::GetSelectedClip()
{
   if (auto affordance = std::dynamic_pointer_cast<WaveTrackAffordanceControls>(GetAffordanceControls()))
   {
      return affordance->GetSelectedClip();
   }
   return {};
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
         bool multi = (display == WaveTrackViewConstants::MultiView);
         if ( multi ) {
            pThis->SetMultiView( true );
            display = WaveTrackSubViewType::Default();
         }

         pThis->DoSetDisplay( display, !multi );
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
