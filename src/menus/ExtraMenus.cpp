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
   static const auto pred =
      []{ return gPrefs->ReadBool(wxT("/GUI/ShowExtraMenus"), false); };
   static auto menu = std::shared_ptr{
      ConditionalItems("Optional", pred,
         Menu("Extra", XXO("Ext&ra"),
            Section("Part1"),
            Section("Part2"))
      )
   };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Indirect(ExtraMenu())
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
         Command( wxT("FullScreenOnOff"), XXO("Enable &Full Screen"),
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
   Indirect(ExtraMiscItems())
};

}
