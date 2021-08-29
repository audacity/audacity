/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackPanelCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "CommonTrackPanelCell.h"

#include <wx/cursor.h>
#include <wx/event.h>

#include "../../widgets/BasicMenu.h"
#include "BasicUI.h"
#include "../../commands/CommandContext.h"
#include "../../commands/CommandManager.h"
#include "../../HitTestResult.h"
#include "../../RefreshCode.h"
#include "../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "../../widgets/wxWidgetsWindowPlacement.h"

CommonTrackPanelCell::~CommonTrackPanelCell()
{
}

HitTestPreview CommonTrackPanelCell::DefaultPreview
(const TrackPanelMouseState &, const AudacityProject *)
{
   static wxCursor defaultCursor{ wxCURSOR_ARROW };
   return { {}, &defaultCursor, {} };
}

auto CommonTrackPanelCell::GetMenuItems(
   const wxRect&, const wxPoint *, AudacityProject * )
      -> std::vector<MenuItem>
{
   return {};
}

unsigned CommonTrackPanelCell::DoContextMenu( const wxRect &rect,
   wxWindow *pParent, const wxPoint *pPoint, AudacityProject *pProject)
{
   const auto items = GetMenuItems( rect, pPoint, pProject );
   if (items.empty())
      return RefreshCode::RefreshNone;

   // Set up command context with extras
   CommandContext context{ *pProject };
   SelectedRegion region;
   if (pPoint) {
      auto time = ViewInfo::Get(*pProject).PositionToTime(pPoint->x, rect.x);
      region = { time, time };
      context.temporarySelection.pSelectedRegion = &region;
   }
   context.temporarySelection.pTrack = FindTrack().get();

   auto &commandManager = CommandManager::Get(*pProject);
   auto flags = MenuManager::Get( *pProject ).GetUpdateFlags();

   BasicMenu::Handle menu{ BasicMenu::FreshMenu };

   for (const auto &item: items) {
      if ( const auto &commandID = item.symbol.Internal();
           commandID.empty() )
         menu.AppendSeparator();
      else {
         // Generate a menu item with the same shortcut key as in the toolbar
         // menu, and as determined by keyboard preferences
         auto label =
            commandManager.FormatLabelForMenu( commandID, &item.symbol.Msgid() );
         menu.Append( label,
            // This lambda will be destroyed with menu
            [&]{
               if (auto &action = item.action)
                  action( context );
               else
                  commandManager.HandleTextualCommand(
                     commandID, context, flags, false); },
            item.action || commandManager.GetEnabled( commandID ) );
      }
   }

   BasicMenu::Point point;
   if (pPoint)
      point = { pPoint->x, pPoint->y };
   menu.Popup(
      wxWidgetsWindowPlacement{ pParent },
      point
   );

   return RefreshCode::RefreshNone;
}

unsigned CommonTrackPanelCell::HandleWheelRotation
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto &hook = MouseWheelHook::Get();
   return hook ? hook( evt, pProject ) : RefreshCode::Cancelled;
}

CommonTrackCell::CommonTrackCell( const std::shared_ptr< Track > &parent )
   : mwTrack { parent }
{}

CommonTrackCell::~CommonTrackCell() = default;

void CommonTrackCell::Reparent( const std::shared_ptr<Track> &parent )
{
   mwTrack = parent;
}

std::shared_ptr<Track> CommonTrackCell::DoFindTrack()
{
   return mwTrack.lock();
}
