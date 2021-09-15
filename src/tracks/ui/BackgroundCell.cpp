/**********************************************************************

Audacity: A Digital Audio Editor

BackgroundCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "BackgroundCell.h"

#include "../../AColor.h"
#include "../../HitTestResult.h"
#include "Project.h"
#include "../../RefreshCode.h"
#include "../../SelectionState.h"
#include "../../Track.h"
#include "../../TrackArtist.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelConstants.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../UIHandle.h"
#include "ViewInfo.h"

#include <wx/cursor.h>
#include <wx/event.h>

// Define this, just so the click to deselect can dispatch here
// This handle class, unlike most, doesn't associate with any particular cell.
class BackgroundHandle : public UIHandle
{
   BackgroundHandle(const BackgroundHandle&) = delete;
   BackgroundHandle &operator=(const BackgroundHandle&) = delete;

public:
   BackgroundHandle() {}

   BackgroundHandle(BackgroundHandle&&) = default;
   BackgroundHandle& operator=(BackgroundHandle&&) = default;

   static HitTestPreview HitPreview()
   {
      static wxCursor arrowCursor{ wxCURSOR_ARROW };
      return { {}, &arrowCursor };
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
         SelectionState::Get( *pProject ).SelectNone(
            TrackList::Get( *pProject ) );
         result |= RefreshAll;
      }

      return result;
   }

   Result Drag
      (const TrackPanelMouseEvent &, AudacityProject *) override
   { return RefreshCode::RefreshNone; }

   HitTestPreview Preview
      (const TrackPanelMouseState &, AudacityProject *) override
   { return HitPreview(); }

   Result Release
      (const TrackPanelMouseEvent &, AudacityProject *,
       wxWindow *) override
   { return RefreshCode::RefreshNone; }

   Result Cancel(AudacityProject *) override
   { return RefreshCode::RefreshNone; }
};

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  []( AudacityProject &parent ){
     auto result = std::make_shared< BackgroundCell >( &parent );
     TrackPanel::Get( parent ).SetBackgroundCell( result );
     return result;
   }
};

BackgroundCell &BackgroundCell::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< BackgroundCell >( key );
}

const BackgroundCell &BackgroundCell::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

BackgroundCell::~BackgroundCell()
{
}

std::vector<UIHandlePtr> BackgroundCell::HitTest
(const TrackPanelMouseState &,
 const AudacityProject *)
{
   auto result = AssignUIHandlePtr(mHandle, std::make_shared<BackgroundHandle>());
   return { result };
}

std::shared_ptr<Track> BackgroundCell::DoFindTrack()
{
   return {};
}

void BackgroundCell::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassBackground ) {
      auto &dc = context.dc;
      // Paint over the part below the tracks
      AColor::TrackPanelBackground( &dc, false );
      dc.DrawRectangle( rect );
   }
}

wxRect BackgroundCell::DrawingArea(
   TrackPanelDrawingContext &,
   const wxRect &rect, const wxRect &, unsigned iPass )
{
   if ( iPass == TrackArtist::PassBackground )
      // If there are any tracks, extend the drawing area up, to cover the
      // bottom ends of any zooming guide lines.
      return {
         rect.x,
         rect.y - kTopMargin,
         rect.width,
         rect.height + kTopMargin
      };
   else
      return rect;
}

auto BackgroundCell::GetMenuItems(
   const wxRect &, const wxPoint *, AudacityProject * )
      -> std::vector<MenuItem>
{
   // These commands exist in toolbar menus too, but maybe with other labels
   // TODO: devise a system of registration so that BackgroundCell has no
   // special knowledge about track sub-types
   return {
      { L"NewMonoTrack", XO("Add Mono Track")},
      { L"NewStereoTrack", XO("Add Stereo Track") },
      { L"NewLabelTrack", XO("Add Label Track"),  },
      {},
      { L"Export", XO("Export Audio..."),  },
      {},
      { L"SelectAll", XO("Select All") },
   };
}
