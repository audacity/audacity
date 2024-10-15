/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackPanelCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "CommonTrackPanelCell.h"

#include <wx/cursor.h>
#include <wx/event.h>
#include <wx/menu.h>

#include "../../widgets/BasicMenu.h"
#include "BasicUI.h"
#include "CommandContext.h"
#include "CommandManager.h"
#include "../../HitTestResult.h"
#include "../../RefreshCode.h"
#include "../../TrackPanelMouseEvent.h"
#include "Track.h"
#include "ViewInfo.h"
#include "wxWidgetsWindowPlacement.h"

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
   wxWindow *pParent, const wxPoint *pPoint, AudacityProject *const pProject)
{
   const auto items = GetMenuItems( rect, pPoint, pProject );
   if (items.empty())
      return RefreshCode::RefreshNone;

   auto &commandManager = CommandManager::Get(*pProject);
   commandManager.UpdateMenus();

   // Set up command context with extras
   CommandContext context{ *pProject };
   SelectedRegion region;
   if (pPoint) {
      auto time = ViewInfo::Get(*pProject).PositionToTime(pPoint->x, rect.x);
      region = { time, time };
      context.temporarySelection.pSelectedRegion = &region;
   }
   context.temporarySelection.pTrack = FindTrack().get();

   auto flags = commandManager.GetUpdateFlags();

   // Common dispatcher for the menu items
   auto dispatcher = [&]( wxCommandEvent &evt ){
      auto idx = evt.GetId() - 1;
      if (idx >= 0 && idx < items.size()) {
         if (auto &action = items[idx].action)
            action( context );
         else
            commandManager.HandleTextualCommand(
               items[idx].symbol.Internal(), context, flags, false);
      }
   };

   wxMenu menu;
   int ii = 1;
   for (const auto &item: items) {
      if ( const auto &commandID = item.symbol.Internal();
           commandID.empty() )
         menu.AppendSeparator();
      else {
         // Generate a menu item with the same shortcut key as in the toolbar
         // menu, and as determined by keyboard preferences
         auto label =
            commandManager.FormatLabelForMenu( commandID, &item.symbol.Msgid() );
         menu.Append( ii, label );
         menu.Bind( wxEVT_COMMAND_MENU_SELECTED, dispatcher );
         bool enabled = item.enabled &&
            (item.action || commandManager.GetEnabled( commandID ));
         menu.Enable( ii, enabled );
      }
      ++ii;
   }
   
   BasicUI::Point point;
   if (pPoint)
      point = { pPoint->x, pPoint->y };
   BasicMenu::Handle{ &menu }.Popup(
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

CommonTrackCell::CommonTrackCell(const std::shared_ptr<Track> &parent)
   : mwTrack{ parent }
{
}

CommonTrackCell::~CommonTrackCell() = default;

void CommonTrackCell::Reparent(const std::shared_ptr<Track> &parent)
{
   mwTrack = parent;
}

std::shared_ptr<Track> CommonTrackCell::DoFindTrack()
{
   return mwTrack.lock();
}

CommonChannelCell::CommonChannelCell(const std::shared_ptr<Channel> &parent)
   : mwChannel{ parent }
{
}

CommonChannelCell::~CommonChannelCell() = default;

void CommonChannelCell::Reparent(
   const std::shared_ptr<Track> &parent, size_t iChannel)
{
   mwChannel = parent->NthChannel(iChannel);
}

std::shared_ptr<Track> CommonChannelCell::DoFindTrack()
{
   Track *pTrack{};
   if (const auto pChannel = mwChannel.lock())
      pTrack = dynamic_cast<Track *>(&pChannel->GetChannelGroup());
   return pTrack ? pTrack->SharedPointer() : nullptr;
}

std::shared_ptr<Channel> CommonChannelCell::DoFindChannel()
{
   return mwChannel.lock();
}

std::shared_ptr<const Channel> CommonChannelCell::DoFindChannel() const
{
   return const_cast<CommonChannelCell*>(this)->FindChannel();
}
