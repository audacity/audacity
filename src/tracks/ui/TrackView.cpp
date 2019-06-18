/**********************************************************************

Audacity: A Digital Audio Editor

TrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackView.h"
#include "../../Track.h"

#include "TrackControls.h"
#include "../../TrackPanelResizerCell.h"

TrackView::~TrackView()
{
}

void TrackView::Copy( const TrackView & )
{
}

TrackView &TrackView::Get( Track &track )
{
   return *track.GetTrackView();
}

const TrackView &TrackView::Get( const Track &track )
{
   return *track.GetTrackView();
}

std::shared_ptr<TrackView> Track::GetTrackView()
{
   if (!mpView)
      // create on demand
      mpView = DoGetView();
   return mpView;
}

std::shared_ptr<const TrackView> Track::GetTrackView() const
{
   return const_cast<Track*>(this)->GetTrackView();
}

std::shared_ptr<TrackPanelCell> Track::GetTrackControls()
{
   if (!mpControls)
      // create on demand
      mpControls = DoGetControls();
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
