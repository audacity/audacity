/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackView.cpp

Paul Licameli split from class TrackView

**********************************************************************/

#include "CommonTrackView.h"

#include "TrackControls.h"

std::vector<UIHandlePtr> CommonTrackView::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   return {};
}

std::shared_ptr<TrackPanelCell> CommonTrackView::ContextMenuDelegate()
{
   return TrackControls::Get( *FindTrack() ).shared_from_this();
}
