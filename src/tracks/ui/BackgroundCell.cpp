/**********************************************************************

Audacity: A Digital Audio Editor

BackgroundCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "BackgroundCell.h"

#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../UIHandle.h"

#include <wx/cursor.h>
#include <wx/event.h>

namespace
{
   // Define this, just so the click to deselect can dispatch here
   class BackgroundHandle : public UIHandle
   {
      BackgroundHandle() {}
      BackgroundHandle(const BackgroundHandle&) = delete;
      BackgroundHandle &operator=(const BackgroundHandle&) = delete;

   public:

      static BackgroundHandle& Instance()
      {
         static BackgroundHandle instance;
         return instance;
      }

      static HitTestPreview HitPreview()
      {
         static wxCursor arrowCursor{ wxCURSOR_ARROW };
         return { {}, &arrowCursor };
      }

      static HitTestResult HitAnywhere()
      {
         return {
            HitPreview(),
            &BackgroundHandle::Instance()
         };
      }

      virtual ~BackgroundHandle()
      {}

      Result Click
         (const TrackPanelMouseEvent &evt, AudacityProject *pProject) override
      {
         using namespace RefreshCode;
         const wxMouseEvent &event = evt.event;
         // Do not start a drag
         Result result = Cancelled;

         // AS: If the user clicked outside all tracks, make nothing
         //  selected.
         if ((event.ButtonDown() || event.ButtonDClick())) {
            pProject->GetSelectionState().SelectNone
               ( *pProject->GetTracks(), pProject->GetMixerBoard() );
            result |= RefreshAll;
         }

         return result;
      }

      Result Drag
         (const TrackPanelMouseEvent &, AudacityProject *) override
      { return RefreshCode::RefreshNone; }

      HitTestPreview Preview
         (const TrackPanelMouseEvent &, const AudacityProject *) override
      { return HitPreview(); }

      Result Release
         (const TrackPanelMouseEvent &, AudacityProject *,
          wxWindow *) override
      { return RefreshCode::RefreshNone; }

      Result Cancel(AudacityProject *) override
      { return RefreshCode::RefreshNone; }
   };
}

BackgroundCell::~BackgroundCell()
{
}

HitTestResult BackgroundCell::HitTest
(const TrackPanelMouseEvent &,
 const AudacityProject *)
{
   return BackgroundHandle::HitAnywhere();
}

std::shared_ptr<Track> BackgroundCell::FindTrack()
{
   return {};
}

