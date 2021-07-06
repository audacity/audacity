/**********************************************************************

Sneedacity: A Digital Audio Editor

BackgroundCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "BackgroundCell.h"

#include "../../AColor.h"
#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../SelectionState.h"
#include "../../Track.h"
#include "../../TrackArtist.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../UIHandle.h"
#include "../../ViewInfo.h"

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

   static HitTestPreview HitPreview()
   {
      static wxCursor arrowCursor{ wxCURSOR_ARROW };
      return { {}, &arrowCursor };
   }

   virtual ~BackgroundHandle()
   {}

   Result Click
      (const TrackPanelMouseEvent &evt, SneedacityProject *pProject) override
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
      (const TrackPanelMouseEvent &, SneedacityProject *) override
   { return RefreshCode::RefreshNone; }

   HitTestPreview Preview
      (const TrackPanelMouseState &, SneedacityProject *) override
   { return HitPreview(); }

   Result Release
      (const TrackPanelMouseEvent &, SneedacityProject *,
       wxWindow *) override
   { return RefreshCode::RefreshNone; }

   Result Cancel(SneedacityProject *) override
   { return RefreshCode::RefreshNone; }
};

static const SneedacityProject::AttachedObjects::RegisteredFactory key{
  []( SneedacityProject &parent ){
     auto result = std::make_shared< BackgroundCell >( &parent );
     TrackPanel::Get( parent ).SetBackgroundCell( result );
     return result;
   }
};

BackgroundCell &BackgroundCell::Get( SneedacityProject &project )
{
   return project.AttachedObjects::Get< BackgroundCell >( key );
}

const BackgroundCell &BackgroundCell::Get( const SneedacityProject &project )
{
   return Get( const_cast< SneedacityProject & >( project ) );
}

BackgroundCell::~BackgroundCell()
{
}

std::vector<UIHandlePtr> BackgroundCell::HitTest
(const TrackPanelMouseState &,
 const SneedacityProject *)
{
   std::vector<UIHandlePtr> results;
   auto result = mHandle.lock();
   if (!result)
      result = std::make_shared<BackgroundHandle>();
   results.push_back(result);
   return results;
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
