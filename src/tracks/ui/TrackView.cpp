/**********************************************************************

Audacity: A Digital Audio Editor

TrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackView.h"
#include "../../Track.h"

#include "../../TrackPanelMouseEvent.h"
#include "TrackControls.h"
#include "../ui/SelectHandle.h"
#include "ZoomHandle.h"
#include "TimeShiftHandle.h"
#include "../../TrackPanelResizerCell.h"
#include "BackgroundCell.h"
#include "../../ProjectSettings.h"

TrackView::~TrackView()
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

std::vector<UIHandlePtr> Track::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   UIHandlePtr result;
   using namespace ToolCodes;
   std::vector<UIHandlePtr> results;
   const auto &settings = ProjectSettings::Get( *pProject );
   const auto currentTool = settings.GetTool();
   const bool isMultiTool = ( currentTool == multiTool );

   if ( !isMultiTool && currentTool == zoomTool ) {
      // Zoom tool is a non-selecting tool that takes precedence in all tracks
      // over all other tools, no matter what detail you point at.
      result = ZoomHandle::HitAnywhere(
         BackgroundCell::Get( *pProject ).mZoomHandle );
      results.push_back(result);
      return results;
   }

   // In other tools, let subclasses determine detailed hits.
   results =
      DetailedHitTest( st, pProject, currentTool, isMultiTool );

   // There are still some general cases.

   // Sliding applies in more than one track type.
   if ( !isMultiTool && currentTool == slideTool ) {
      result = TimeShiftHandle::HitAnywhere(
         mTimeShiftHandle, SharedPointer(), false);
      if (result)
         results.push_back(result);
   }

   // Let the multi-tool right-click handler apply only in default of all
   // other detailed hits.
   if ( isMultiTool ) {
      result = ZoomHandle::HitTest(
         BackgroundCell::Get( *pProject ).mZoomHandle, st.state);
      if (result)
         results.push_back(result);
   }

   // Finally, default of all is adjustment of the selection box.
   if ( isMultiTool || currentTool == selectTool ) {
      result = SelectHandle::HitTest(
         mSelectHandle, st, pProject, SharedPointer());
      if (result)
         results.push_back(result);
   }

   return results;
}

std::shared_ptr<TrackPanelCell> Track::ContextMenuDelegate()
{
   return TrackControls::Get( *this ).shared_from_this();
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
std::shared_ptr<TrackPanelCell> Track::GetResizer()
{
   if (!mpResizer)
      // create on demand
      mpResizer = std::make_shared<TrackPanelResizerCell>( SharedPointer() );
   return mpResizer;
}
