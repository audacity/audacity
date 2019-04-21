#include "../Audacity.h"

#include "../commands/CommandManager.h"

// This file collects a few things specific to Mac and requiring some
// Objective-C++ .  Avoid mixing languages elsewhere.

#ifdef __WXMAC__

#include "../AudacityApp.h"
#include "../Menus.h"
#include "../Project.h"
#include "../commands/CommandContext.h"

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
   auto window = project;
   if (window) {
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
      GetMenuManager(*project).UpdateMenus(*project);
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
   auto window = &context.project;
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
   for (const auto project : gAudacityProjects) {
      project->Raise();
   }
}

void OnMacMinimizeAll(const CommandContext &)
{
   for (const auto project : gAudacityProjects) {
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

#define FN(X) findCommandHandler, \
   static_cast<CommandFunctorPointer>(& WindowActions::Handler :: X)
#define XXO(X) _(X), wxString{X}.Contains("...")

MenuTable::BaseItemPtr WindowMenu( AudacityProject & )
{
      //////////////////////////////////////////////////////////////////////////
      // poor imitation of the Mac Windows Menu
      //////////////////////////////////////////////////////////////////////////
   using namespace MenuTable;
   return Menu( _("&Window"),
      /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
       * window) shrink to an icon on the dock */
      Command( wxT("MacMinimize"), XXO("&Minimize"), FN(OnMacMinimize),
         NotMinimizedFlag, wxT("Ctrl+M") ),
      /* i18n-hint: Standard Macintosh Window menu item:  Make (the current
       * window) full sized */
      Command( wxT("MacZoom"), XXO("&Zoom"),
         FN(OnMacZoom), NotMinimizedFlag ),

      Separator(),

      /* i18n-hint: Standard Macintosh Window menu item:  Make all project
       * windows un-hidden */
      Command( wxT("MacBringAllToFront"), XXO("&Bring All to Front"),
         FN(OnMacBringAllToFront), AlwaysEnabledFlag )
   );
}

MenuTable::BaseItemPtr ExtraWindowItems( AudacityProject & )
{
   using namespace MenuTable;

   return Items(
      /* i18n-hint: Shrink all project windows to icons on the Macintosh
         tooldock */
      Command( wxT("MacMinimizeAll"), XXO("Minimize All Projects"),
         FN(OnMacMinimizeAll),
         AlwaysEnabledFlag, wxT("Ctrl+Alt+M") )
   );
}

#undef XXO
#undef FN

// One more Objective C++ function for another class scope, kept in this file

void AudacityApp::MacActivateApp()
{
   id app = [NSApplication sharedApplication];
   if ( [app respondsToSelector:@selector(activateIgnoringOtherApps:)] )
      [app activateIgnoringOtherApps:YES];
}

#else

// Not WXMAC.  Stub functions.
MenuTable::BaseItemPtr WindowMenu( AudacityProject & )
{
   return nullptr;
}

MenuTable::BaseItemPtr ExtraWindowItems( AudacityProject & )
{
   return nullptr;
}

#endif
