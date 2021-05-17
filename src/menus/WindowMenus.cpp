

#include "../commands/CommandManager.h"

// This file collects a few things specific to Mac and requiring some
// Objective-C++ .  Avoid mixing languages elsewhere.

// Only needed when using wxWidgets < 3.1.3
#if defined(__WXMAC__) && !wxCHECK_VERSION(3,1,3)

#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "../Project.h"
#include "../commands/CommandContext.h"

#include <wx/frame.h>
#include <wx/menu.h>

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

std::vector< wxWindowID > sReservedIds;
std::vector< std::weak_ptr< AudacityProject > > sProjects;

void RebuildMenu(wxCommandEvent &evt)
{
   // Let other listeners hear it too
   evt.Skip();
   
   // This is a big hammer.
   // Really we just need to recreate just the Window menu.
   // This causes the checkmark to be put in the right place for the
   // currently active project
   MenuCreator::RebuildAllMenuBars();
}

wxWindowID ReservedID(
   size_t index, const std::shared_ptr< AudacityProject > &pProject )
{
   if ( sReservedIds.empty() ) {
      // Do this once only per session, and don't worry about unbinding
      wxTheApp->Bind( EVT_PROJECT_ACTIVATION, RebuildMenu );
      wxTheApp->Bind( EVT_PROJECT_TITLE_CHANGE, RebuildMenu );
   }

   while ( sReservedIds.size() <= index )
      sReservedIds.emplace_back( wxIdManager::ReserveId() );

   if ( sProjects.size() < sReservedIds.size() )
      sProjects.resize( sReservedIds.size() );
   sProjects[ index ] = pProject;

   return sReservedIds[ index ];
}

void OnWindow( wxCommandEvent &evt )
{
   const auto begin = sReservedIds.begin(), end = sReservedIds.end(),
      iter = std::find( begin, end, evt.GetId() );
   size_t index = iter - begin;
   if ( index < sProjects.size() ) {
      auto pProject = sProjects[ index ].lock();
      if ( pProject ) {
         // Make it the active project
         SetActiveProject(pProject.get());

         // And ensure it's visible
         wxFrame *frame = pProject->GetFrame();
         if (frame->IsIconized())
         {
            frame->Restore();
         }
         frame->Raise();
      }
   }
}

} // namespace

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
   // Really this de-miniaturizes all, which is not exactly the standard
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
   Menu( wxT("Window"), XXO("&Window"),
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
            // Undo previous bindings
            for ( auto id : sReservedIds )
               wxTheApp->Unbind( wxEVT_MENU, OnWindow, id );

            // Add all projects to this project's Window menu
            size_t ii = 0;
            for (auto project : AllProjects{})
            {
               int itemId = ReservedID( ii++, project );
               wxString itemName = project->GetFrame()->GetTitle();
               bool isActive = (GetActiveProject() == project.get());

               // This should never really happen, but a menu item must have a name
               if (itemName.empty())
               {
                  itemName = _("<untitled>");
               }

               // Add it to the menu and check it if it's the active project
               wxMenuItem *item = theMenu.Append(itemId, itemName);
               item->SetCheckable(true);
               item->Check(isActive);

               // Bind the callback
               wxTheApp->Bind( wxEVT_MENU, OnWindow, itemId );
            }
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
