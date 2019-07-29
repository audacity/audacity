#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectWindows.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../toolbars/ToolManager.h"

#include <wx/frame.h>

// helper functions and classes
namespace {

// Menu handler functions

void OnFullScreen(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = GetProjectFrame( project );

   bool bChecked = !window.wxTopLevelWindow::IsFullScreen();
   window.wxTopLevelWindow::ShowFullScreen(bChecked);

   ToolManager::Get(project).ModifyToolbarMenus(project);
}

// Menu definitions

using namespace MenuTable;

BaseItemSharedPtr ExtraMenu()
{
   // Table of menu factories.
   static BaseItemSharedPtr extraItems{ Items( wxEmptyString,
      Section( "Part1" ),
      Section( "Part2" )
   ) };

   static const auto pred =
      []{ return gPrefs->ReadBool(wxT("/GUI/ShowExtraMenus"), false); };
   static BaseItemSharedPtr menu{
      ConditionalItems( wxT("Optional"),
         pred, Menu( wxT("Extra"), XXO("Ext&ra"), extraItems ) )
   };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( ExtraMenu() )
};

// Under /MenuBar/Optional/Extra/Part2
BaseItemSharedPtr ExtraMiscItems()
{
   using Options = CommandManager::Options;

   // Not a menu.
   static BaseItemSharedPtr items{
   Items( wxT("Misc"),
      // Delayed evaluation
      []( AudacityProject &project ) {

   static const auto key =
#ifdef __WXMAC__
      wxT("Ctrl+/")
#else
      wxT("F11")
#endif
   ;

         return (
         // Accel key is not bindable.
         Command( wxT("FullScreenOnOff"), XXO("&Full Screen (on/off)"),
            OnFullScreen,
            AlwaysEnabledFlag,
            Options{ key }.CheckTest( []( const AudacityProject &project ) {
               return GetProjectFrame( project )
                  .wxTopLevelWindow::IsFullScreen(); } ) )
        );
      }
   ) };
   return items;
}

AttachedItem sAttachment2{
   Placement{ wxT("Optional/Extra/Part2"), { OrderingHint::End } },
   Shared( ExtraMiscItems() )
};

}
