//
//  MenusMac.cpp
//  Audacity
//
//  Created by Paul Licameli on 6/28/16.
//
//

// This file collects a few things specific to Mac and requiring some
// Objective-C++ .  Avoid mixing languages elsewhere.

#include "Audacity.h"
#include "AudacityApp.h"
#include "Menus.h"
#include "Project.h"
#include "commands/CommandContext.h"

#undef USE_COCOA

#ifdef USE_COCOA
#include <AppKit/AppKit.h>
#include <wx/osx/private.h>
#endif

#include <objc/objc.h>
#include <CoreFoundation/CoreFoundation.h>

void MenuCommandHandler::DoMacMinimize(AudacityProject *project)
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

void MenuCommandHandler::OnMacMinimize(const CommandContext &context)
{
   DoMacMinimize(&context.project);
}

void MenuCommandHandler::OnMacMinimizeAll(const CommandContext &)
{
   for (const auto project : gAudacityProjects) {
      DoMacMinimize(project.get());
   }
}

void MenuCommandHandler::OnMacZoom(const CommandContext &context)
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

void MenuCommandHandler::OnMacBringAllToFront(const CommandContext &)
{
   // Reall this de-miniaturizes all, which is not exactly the standard
   // behavior.
   for (const auto project : gAudacityProjects) {
      project->Raise();
   }
}

void AudacityApp::MacActivateApp()
{
   id app = [NSApplication sharedApplication];
   if ( [app respondsToSelector:@selector(activateIgnoringOtherApps:)] )
      [app activateIgnoringOtherApps:YES];
}
