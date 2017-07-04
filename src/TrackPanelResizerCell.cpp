/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelResizeHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "Audacity.h"
#include "TrackPanelResizerCell.h"
#include "TrackPanelResizeHandle.h"
#include "HitTestResult.h"

TrackPanelResizerCell::TrackPanelResizerCell( std::shared_ptr<Track> pTrack )
: mpTrack{ pTrack }
{}

HitTestResult TrackPanelResizerCell::HitTest
(const TrackPanelMouseEvent &event, const AudacityProject *pProject)
{
   return {
      TrackPanelResizeHandle::HitPreview( mBetweenTracks ),
      &TrackPanelResizeHandle::Instance()
   };
}
