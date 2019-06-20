/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "LabelTrackView.h"
#include "../../../LabelTrack.h"

#include "LabelTrackControls.h"
#include "LabelTrackVRulerControls.h"
#include "LabelGlyphHandle.h"
#include "LabelTextHandle.h"
#include "LabelTrackVRulerControls.h"

#include "../../../HitTestResult.h"
#include "../../../TrackPanelMouseEvent.h"

LabelTrackView::LabelTrackView( const std::shared_ptr<Track> &pTrack )
   : CommonTrackView{ pTrack }
{
   // Label tracks are narrow
   // Default is to allow two rows so that NEW users get the
   // idea that labels can 'stack' when they would overlap.
   DoSetHeight(73);

   ResetFont();
   CreateCustomGlyphs();
   ResetFlags();

   // Events will be emitted by the track
   const auto pLabelTrack = FindLabelTrack();
   BindTo( pLabelTrack.get() );
}

LabelTrackView::~LabelTrackView()
{
}

void LabelTrackView::Reparent( const std::shared_ptr<Track> &parent )
{
   auto oldParent = FindLabelTrack();
   auto newParent = track_cast<LabelTrack*>(parent.get());
   if (oldParent.get() != newParent) {
      UnbindFrom( oldParent.get() );
      BindTo( newParent );
   }
   CommonTrackView::Reparent( parent );
}

void LabelTrackView::BindTo( LabelTrack *pParent )
{
   pParent->Bind(
      EVT_LABELTRACK_ADDITION, &LabelTrackView::OnLabelAdded, this );
   pParent->Bind(
      EVT_LABELTRACK_DELETION, &LabelTrackView::OnLabelDeleted, this );
   pParent->Bind(
      EVT_LABELTRACK_PERMUTED, &LabelTrackView::OnLabelPermuted, this );
}

void LabelTrackView::UnbindFrom( LabelTrack *pParent )
{
   pParent->Unbind(
      EVT_LABELTRACK_ADDITION, &LabelTrackView::OnLabelAdded, this );
   pParent->Unbind(
      EVT_LABELTRACK_DELETION, &LabelTrackView::OnLabelDeleted, this );
   pParent->Unbind(
      EVT_LABELTRACK_PERMUTED, &LabelTrackView::OnLabelPermuted, this );
}

void LabelTrackView::Copy( const TrackView &other )
{
   TrackView::Copy( other );

   if ( const auto pOther = dynamic_cast< const LabelTrackView* >( &other ) ) {
      // only one field is important to preserve in undo/redo history
      mSelIndex = pOther->mSelIndex;
   }
}

LabelTrackView &LabelTrackView::Get( LabelTrack &track )
{
   return static_cast< LabelTrackView& >( TrackView::Get( track ) );
}

const LabelTrackView &LabelTrackView::Get( const LabelTrack &track )
{
   return static_cast< const LabelTrackView& >( TrackView::Get( track ) );
}

std::shared_ptr<LabelTrack> LabelTrackView::FindLabelTrack()
{
   return std::static_pointer_cast<LabelTrack>( FindTrack() );
}

std::shared_ptr<const LabelTrack> LabelTrackView::FindLabelTrack() const
{
   return const_cast<LabelTrackView*>(this)->FindLabelTrack();
}

std::vector<UIHandlePtr> LabelTrackView::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *WXUNUSED(pProject), int, bool)
{
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
   const wxMouseState &state = st.state;

   const auto pTrack = FindLabelTrack();
   result = LabelGlyphHandle::HitTest(
      mGlyphHandle, state, pTrack, st.rect);
   if (result)
      results.push_back(result);

   result = LabelTextHandle::HitTest(
      mTextHandle, state, pTrack);
   if (result)
      results.push_back(result);

   return results;
}

std::shared_ptr<TrackView> LabelTrack::DoGetView()
{
   return std::make_shared<LabelTrackView>( SharedPointer() );
}

std::shared_ptr<TrackControls> LabelTrack::DoGetControls()
{
   return std::make_shared<LabelTrackControls>( SharedPointer() );
}

std::shared_ptr<TrackVRulerControls> LabelTrackView::DoGetVRulerControls()
{
   return
      std::make_shared<LabelTrackVRulerControls>( shared_from_this() );
}
