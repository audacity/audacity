//
//  MenusMac.cpp
//  Audacity
//
//  Created by Paul Licameli on 6/28/16.
//
//

#include "Audacity.h"
#include "Project.h"

#undef USE_COCOA

#ifdef USE_COCOA
#include <AppKit/AppKit.h>
#include <wx/osx/private.h>
#endif

void AudacityProject::DoMacMinimize(AudacityProject *project)
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
      project->UpdateMenus();
   }
}

void AudacityProject::OnMacMinimize()
{
   DoMacMinimize(this);
}

void AudacityProject::OnMacMinimizeAll()
{
   for (const auto project : gAudacityProjects) {
      DoMacMinimize(project.get());
   }
}

void AudacityProject::OnMacZoom()
{
   auto window = this;
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

void AudacityProject::OnMacBringAllToFront()
{
   // Reall this de-miniaturizes all, which is not exactly the standard
   // behavior.
   for (const auto project : gAudacityProjects) {
      project->Raise();
   }
}
