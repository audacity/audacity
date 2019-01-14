#include "../Audacity.h"

#include "../commands/CommandManager.h"

// This file collects a few things specific to Mac and requiring some
// Objective-C++ .  Avoid mixing languages elsewhere.

#ifdef __WXMAC__

#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "../Project.h"
#include "../commands/CommandContext.h"

#include <wx/frame.h>

#undef USE_COCOA

#ifdef USE_COCOA
#include <AppKit/AppKit.h>
#include <wx/osx/private.h>
#endif

#include <AppKit/NSApplication.h>

#include <objc/objc.h>
#include <CoreFoundation/CoreFoundation.h>

// private helper classes and functions
namespace {

void DoMacMinimize(AudacityProject *project)
{
   if (project) {
      auto window = &GetProjectFrame( *project );
#ifdef USE_COCOA
      // Adapted from mbarman.mm in wxWidgets 3.0.2
      auto peer = window->GetPeer();
      peer->GetWXPeer();
      auto widget = static_cast<wxWidgetCocoaImpl*>(peer)->GetWXWidget();
      auto nsWindow = [widget window];
      if (nsWindow) {
         [nsWindow performMiniaturize:widget];
      }
#else
      window->Iconize(true);
#endif

      // So that the Minimize menu command disables
      MenuManager::Get(*project).UpdateMenus();
   }
}

}

/// Namespace for functions for window management (mac only?)
namespace WindowActions {

// exported helper functions
// none

// Menu handler functions

struct Handler : CommandHandlerObject {

void OnMacMinimize(const CommandContext &context)
{
   DoMacMinimize(&context.project);
}

void OnMacZoom(const CommandContext &context)
{
   auto window = &GetProjectFrame( context.project );
   auto topWindow = static_cast<wxTopLevelWindow*>(window);
   auto maximized = topWindow->IsMaximized();
   if (window) {
#ifdef USE_COCOA
      // Adapted from mbarman.mm in wxWidgets 3.0.2
      auto peer = window->GetPeer();
      peer->GetWXPeer();
      auto widget = static_cast<wxWidgetCocoaImpl*>(peer)->GetWXWidget();
      auto nsWindow = [widget window];
      if (nsWindow)
         [nsWindow performZoom:widget];
#else
      topWindow->Maximize(!maximized);
#endif
   }
}

void OnMacBringAllToFront(const CommandContext &)
{
   // Reall this de-miniaturizes all, which is not exactly the standard
   // behavior.
   for (const auto project : AllProjects{})
      GetProjectFrame( *project ).Raise();
}

void OnMacMinimizeAll(const CommandContext &)
{
   for (const auto project : AllProjects{}) {
      DoMacMinimize(project.get());
   }
}

}; // struct Handler

} // namespace

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static WindowActions::Handler instance;
   return instance;
};

// Menu definitions

#define FN(X) (& WindowActions::Handler :: X)

namespace {
using namespace MenuTable;
BaseItemSharedPtr WindowMenu()
{
      //////////////////////////////////////////////////////////////////////////
      // poor imitation of the Mac Windows Menu
      //////////////////////////////////////////////////////////////////////////
   static BaseItemSharedPtr menu{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Window"), XO("&Window"),
      Section( "",
         /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
          * window) shrink to an icon on the dock */
         Command( wxT("MacMinimize"), XXO("&Minimize"), FN(OnMacMinimize),
            NotMinimizedFlag(), wxT("Ctrl+M") ),
         /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
          * window) full sized */
         Command( wxT("MacZoom"), XXO("&Zoom"),
            FN(OnMacZoom), NotMinimizedFlag() )
      ),

      Section( "",
         /* i18n-hint: Standard Macintosh Window menu item:  Make all project
          * windows un-hidden */
         Command( wxT("MacBringAllToFront"), XXO("&Bring All to Front"),
            FN(OnMacBringAllToFront), AlwaysEnabledFlag )
      )
   ) ) };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( WindowMenu() )
};

}

// Under /MenuBar/Optional/Extra/Misc
MenuTable::BaseItemSharedPtr ExtraWindowItems()
{
   static BaseItemSharedPtr items{
   ( FinderScope{ findCommandHandler },
   Items( wxT("MacWindows"),
      /* i18n-hint: Shrink all project windows to icons on the Macintosh
         tooldock */
      Command( wxT("MacMinimizeAll"), XXO("Minimize All Projects"),
         FN(OnMacMinimizeAll),
         AlwaysEnabledFlag, wxT("Ctrl+Alt+M") )
   ) ) };
   return items;
}

#undef FN

#else

// Not WXMAC.  Stub functions.
MenuTable::BaseItemSharedPtr ExtraWindowItems()
{
   return nullptr;
}

#endif
