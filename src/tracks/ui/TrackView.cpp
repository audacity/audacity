/**********************************************************************

Audacity: A Digital Audio Editor

TrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackView.h"
#include "../../Track.h"

#include "TrackControls.h"
#include "../../TrackPanelResizerCell.h"

#include "../../ClientData.h"
#include "../../Project.h"
#include "../../xml/XMLTagHandler.h"
#include "../../xml/XMLWriter.h"

TrackView::~TrackView()
{
}

int TrackView::GetTrackHeight( const Track *pTrack )
{
   return pTrack ? Get( *pTrack ).GetHeight() : 0;
}

int TrackView::GetChannelGroupHeight( const Track *pTrack )
{
   return pTrack ? TrackList::Channels( pTrack ).sum( GetTrackHeight ) : 0;
}

int TrackView::GetCumulativeHeight( const Track *pTrack )
{
   if ( !pTrack )
      return 0;
   auto &view = Get( *pTrack );
   return view.GetY() + view.GetHeight();
}

int TrackView::GetTotalHeight( const TrackList &list )
{
   return GetCumulativeHeight( *list.Any().rbegin() );
}

void TrackView::Copy( const CommonTrackCell &otherCell )
{
   auto pOther = dynamic_cast< const TrackView* >( &otherCell );
   if ( !pOther )
      return;
   auto &other = *pOther;

   mMinimized = other.mMinimized;

   // Let mY remain 0 -- TrackPositioner corrects it later
   mY = 0;
   mHeight = other.mHeight;
}

TrackView &TrackView::Get( Track &track )
{
   return static_cast<TrackView&>( *track.GetTrackView() );
}

const TrackView &TrackView::Get( const Track &track )
{
   return static_cast<const TrackView&>( *track.GetTrackView() );
}

void TrackView::SetMinimized(bool isMinimized)
{
   // Do special changes appropriate to subclass
   DoSetMinimized(isMinimized);

   // Update positions and heights starting from the first track in the group
   auto leader = *TrackList::Channels( FindTrack().get() ).begin();
   if ( leader )
      leader->AdjustPositions();
}

void TrackView::WriteXMLAttributes( XMLWriter &xmlFile ) const
{
   xmlFile.WriteAttr(wxT("height"), GetActualHeight());
   xmlFile.WriteAttr(wxT("minimized"), GetMinimized());
}

bool TrackView::HandleXMLAttribute( const wxChar *attr, const wxChar *value )
{
   wxString strValue;
   long nValue;
   if (!wxStrcmp(attr, wxT("height")) &&
         XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) {
      SetHeight(nValue);
      return true;
   }
   else if (!wxStrcmp(attr, wxT("minimized")) &&
         XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) {
      SetMinimized(nValue != 0);
      return true;
   }
   else
      return false;
}

void TrackView::DoSetMinimized(bool isMinimized)
{
   mMinimized = isMinimized;
}

std::shared_ptr<CommonTrackCell> Track::GetTrackView()
{
   if (!mpView)
      // create on demand
      mpView = DoGetView::Call( *this );
   return mpView;
}

std::shared_ptr<const CommonTrackCell> Track::GetTrackView() const
{
   return const_cast<Track*>(this)->GetTrackView();
}

std::shared_ptr<TrackPanelCell> Track::GetTrackControls()
{
   if (!mpControls)
      // create on demand
      mpControls = DoGetControls::Call( *this );
   return mpControls;
}

std::shared_ptr<const TrackPanelCell> Track::GetTrackControls() const
{
   return const_cast< Track* >( this )->GetTrackControls();
}

std::shared_ptr<TrackVRulerControls> TrackView::GetVRulerControls()
{
   if (!mpVRulerControls)
      // create on demand
      mpVRulerControls = DoGetVRulerControls();
   return mpVRulerControls;
}

std::shared_ptr<const TrackVRulerControls> TrackView::GetVRulerControls() const
{
   return const_cast< TrackView* >( this )->GetVRulerControls();
}

#include "../../TrackPanelResizeHandle.h"
std::shared_ptr<TrackPanelCell> TrackView::GetResizer()
{
   if (!mpResizer)
      // create on demand
      mpResizer = std::make_shared<TrackPanelResizerCell>( shared_from_this() );
   return mpResizer;
}

std::shared_ptr<const TrackPanelCell> TrackView::GetResizer() const
{
   return const_cast<TrackView*>(this)->GetResizer();
}

void TrackView::DoSetY(int y)
{
   mY = y;
}

int TrackView::GetHeight() const
{
   if ( GetMinimized() )
      return GetMinimizedHeight();

   return mHeight;
}

void TrackView::SetHeight(int h)
{
   DoSetHeight(h);
   FindTrack()->AdjustPositions();
}

void TrackView::DoSetHeight(int h)
{
   mHeight = h;
}

namespace {

// Attach an object to each project.  It receives track list events and updates
// track Y coordinates
struct TrackPositioner : ClientData::Base, wxEvtHandler
{
   AudacityProject &mProject;

   explicit TrackPositioner( AudacityProject &project )
      : mProject{ project }
   {
      TrackList::Get( project ).Bind(
         EVT_TRACKLIST_ADDITION, &TrackPositioner::OnUpdate, this );
      TrackList::Get( project ).Bind(
         EVT_TRACKLIST_DELETION, &TrackPositioner::OnUpdate, this );
      TrackList::Get( project ).Bind(
         EVT_TRACKLIST_PERMUTED, &TrackPositioner::OnUpdate, this );
      TrackList::Get( project ).Bind(
         EVT_TRACKLIST_RESIZING, &TrackPositioner::OnUpdate, this );
   }

   void OnUpdate( TrackListEvent & e )
   {
      e.Skip();

      auto iter =
         TrackList::Get( mProject ).Find( e.mpTrack.lock().get() );
      if ( !*iter )
         return;

      auto prev = iter;
      auto yy = TrackView::GetCumulativeHeight( *--prev );

      while( auto pTrack = *iter ) {
         auto &view = TrackView::Get( *pTrack );
         view.SetY( yy );
         yy += view.GetHeight();
         ++iter;
      }
   }
};

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  []( AudacityProject &project ){
     return std::make_shared< TrackPositioner >( project );
   }
};

}
