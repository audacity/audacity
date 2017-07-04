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

UIHandlePtr TrackPanelResizerCell::HitTest
(const TrackPanelMouseState &st, const AudacityProject *pProject)
{
   auto pTrack = mpTrack.lock();
   if (pTrack) {
      auto result = std::make_shared<TrackPanelResizeHandle>(
         pTrack, st.state.m_y, pProject );
      result = AssignUIHandlePtr(mResizeHandle, result);
      return result;
   }
   return {};
}
