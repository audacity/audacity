/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackPanelCell.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "CommonTrackPanelCell.h"

#include <wx/cursor.h>
#include <wx/event.h>
#include <wx/menu.h>

#include "../../commands/CommandContext.h"
#include "../../commands/CommandManager.h"
#include "../../HitTestResult.h"
#include "../../RefreshCode.h"
#include "../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"

namespace {
   CommonTrackPanelCell::Hook &GetHook()
   {
      static CommonTrackPanelCell::Hook theHook;
      return theHook;
   }
}

auto CommonTrackPanelCell::InstallMouseWheelHook( const Hook &hook )
   -> Hook
{
   auto &theHook = GetHook();
   auto result = theHook;
   theHook = hook;
   return result;
}

CommonTrackPanelCell::~CommonTrackPanelCell()
{
}

HitTestPreview CommonTrackPanelCell::DefaultPreview
(const TrackPanelMouseState &, const AudacityProject *)
{
   static wxCursor defaultCursor{ wxCURSOR_ARROW };
   return { {}, &defaultCursor, {} };
}

std::vector<ComponentInterfaceSymbol> CommonTrackPanelCell::GetMenuItems(
   const wxRect&, const wxPoint *, AudacityProject * )
{
   return {};
}

unsigned CommonTrackPanelCell::DoContextMenu( const wxRect &rect,
   wxWindow *pParent, const wxPoint *pPoint, AudacityProject *pProject)
{
   const auto symbols = GetMenuItems( rect, pPoint, pProject );
   if (symbols.empty())
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

   // Common dispatcher for the menu items
   auto dispatcher = [&]( wxCommandEvent &evt ){
      auto idx = evt.GetId() - 1;
      if (idx >= 0 && idx < symbols.size()) {
         commandManager.HandleTextualCommand(
            symbols[idx].Internal(), context, flags, false);
      }
   };

   wxMenu menu;
   int ii = 1;
   for (const auto &symbol: symbols) {
      if ( symbol.Internal().empty() )
         menu.AppendSeparator();
      else {
         // Generate a menu item with the same shortcut key as in the toolbar
         // menu, and as determined by keyboard preferences
         const auto &commandID = symbol.Internal();
         auto label =
            commandManager.FormatLabelForMenu( commandID, &symbol.Msgid() );
         menu.Append( ii, label );
         menu.Bind( wxEVT_COMMAND_MENU_SELECTED, dispatcher );
         menu.Enable( ii, commandManager.GetEnabled( commandID ) );
      }
      ++ii;
   }
   
   pParent->PopupMenu(&menu, pPoint ? *pPoint : wxDefaultPosition);

   return RefreshCode::RefreshNone;
}

unsigned CommonTrackPanelCell::HandleWheelRotation
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   auto hook = GetHook();
   return hook ? hook( evt, pProject ) : RefreshCode::Cancelled;
}

CommonTrackCell::CommonTrackCell( const std::shared_ptr< Track > &parent )
   : mwTrack { parent }
{}

CommonTrackCell::~CommonTrackCell() = default;

void CommonTrackCell::CopyTo( Track& ) const
{
}

void CommonTrackCell::Reparent( const std::shared_ptr<Track> &parent )
{
   mwTrack = parent;
}

std::shared_ptr<Track> CommonTrackCell::DoFindTrack()
{
   return mwTrack.lock();
}

void CommonTrackCell::WriteXMLAttributes( XMLWriter & ) const
{
}

bool CommonTrackCell::HandleXMLAttribute( const wxChar *, const wxChar * )
{
   return false;
}
