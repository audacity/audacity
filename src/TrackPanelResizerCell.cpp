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

#include <wx/mousestate.h>

TrackPanelResizerCell::TrackPanelResizerCell( std::shared_ptr<Track> pTrack )
: mpTrack{ pTrack }
{}

std::vector<UIHandlePtr> TrackPanelResizerCell::HitTest
(const TrackPanelMouseState &st, const AudacityProject *pProject)
{
   (void)pProject;// Compiler food
   std::vector<UIHandlePtr> results;
   auto pTrack = mpTrack.lock();
   if (pTrack) {
      auto result = std::make_shared<TrackPanelResizeHandle>(
         pTrack, st.state.m_y );
      result = AssignUIHandlePtr(mResizeHandle, result);
      results.push_back(result);
   }
   return results;
}
