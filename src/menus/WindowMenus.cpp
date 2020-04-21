#include "../Audacity.h"

#include "../commands/CommandManager.h"

// This file collects a few things specific to Mac and requiring some
// Objective-C++ .  Avoid mixing languages elsewhere.

#ifdef __WXMAC__

#include "WindowMenus.h"

#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "../Project.h"
#include "../commands/CommandContext.h"

#include <wx/frame.h>
#include <wx/menu.h>
#include <wx/menuitem.h>

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

const auto menuTitle = XO("&Window");

}

/// Namespace for functions for window management (mac only?)
namespace WindowActions {

// exported helper functions

// Refreshes the Window menu in all projects
void Refresh()
{
   // Must do it in all projects
   for (auto thisProject : AllProjects{})
   {
      // Need the projects frame, but this should always be successful
      wxFrame *frame = thisProject->GetFrame();
      wxASSERT(frame != NULL);
      if (!frame)
      {
         continue;
      }

      // This can happen if we are called before the menubar is set in the frame
      wxMenuBar *bar = frame->GetMenuBar();
      if (!bar)
      {
         continue;
      }

      // Should always find the Window menu
      int pos = bar->FindMenu( menuTitle.Translation() );
      wxASSERT(pos != wxNOT_FOUND);
      if (pos == wxNOT_FOUND)
      {
         continue;
      }

      // We can not get the menu proper
      wxMenu *menu = bar->GetMenu(pos);

      // Remove all existing window items
      for (auto item : menu->GetMenuItems())
      {
         if (item->GetId() >= WindowActions::ID_BASE)
         {
            menu->Destroy(item);
         }
      }

      // Add all projects to this project's Window menu
      for (auto project : AllProjects{})
      {
         int itemId = WindowActions::ID_BASE + project->GetProjectNumber();
         wxString itemName = project->GetFrame()->GetTitle();
         bool isActive = (GetActiveProject() == project.get());

         // This should never really happen, but a menu item must have a name
         if (itemName.empty())
         {
            itemName = _("<untitled>");
         }

         // Add it to the menu and check it if it's the active project
         wxMenuItem *item = menu->Append(itemId, itemName);
         item->SetCheckable(true);
         item->Check(isActive);
      }
   }
}

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
   Menu( wxT("Window"), menuTitle,
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
      ),

      Section( "",
         Special( wxT("PopulateWindowsStep"),
         [](AudacityProject &, wxMenu &theMenu)
         {
            // Should something be done here???
         } )
      )

   ) ) };
   return menu;
}

AttachedItem sAttachment1{
   wxT(""),
   Shared( WindowMenu() )
};

BaseItemSharedPtr ExtraWindowItems()
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

AttachedItem sAttachment2{
   Placement{ wxT("Optional/Extra/Part2/Misc"), OrderingHint::End },
   Shared( ExtraWindowItems() )
};

}

#undef FN

#else

// Not WXMAC.

#endif
