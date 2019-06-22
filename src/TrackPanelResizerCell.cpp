/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelResizeHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "Audacity.h"
#include "TrackPanelResizerCell.h"

#include "TrackPanelResizeHandle.h"
#include "TrackPanelMouseEvent.h"
#include "HitTestResult.h"

#include "tracks/ui/TrackView.h"

#include <wx/mousestate.h>

TrackPanelResizerCell::TrackPanelResizerCell(
   const std::shared_ptr<TrackView> &pView )
   : mwView{ pView }
{}

std::vector<UIHandlePtr> TrackPanelResizerCell::HitTest
(const TrackPanelMouseState &st, const AudacityProject *pProject)
{
   (void)pProject;// Compiler food
   std::vector<UIHandlePtr> results;
   auto pTrack = FindTrack();
   if (pTrack) {
      auto result = std::make_shared<TrackPanelResizeHandle>(
         pTrack, st.state.m_y );
      result = AssignUIHandlePtr(mResizeHandle, result);
      results.push_back(result);
   }
   return results;
}

std::shared_ptr<Track> TrackPanelResizerCell::DoFindTrack()
{
   const auto pView = mwView.lock();
   if ( pView )
      return pView->FindTrack();
   return {};
}
