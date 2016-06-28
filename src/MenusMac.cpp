//
//  MenusMac.cpp
//  Audacity
//
//  Created by Paul Licameli on 6/28/16.
//
//

#include "Audacity.h"
#include "Project.h"

#include <AppKit/AppKit.h>
#include <wx/osx/private.h>

void AudacityProject::OnMacMinimize()
{
   auto window = wxWindow::FindFocus();
   while (window && ! window->IsTopLevel())
      window = window->GetParent();
   if (window) {
      auto peer = window->GetPeer();
      peer->GetWXPeer();
      auto widget = static_cast<wxWidgetCocoaImpl*>(peer)->GetWXWidget();
      auto nsWindow = [widget window];
      if (nsWindow) {
         [nsWindow performMiniaturize:widget];
      }
      if (nsWindow) {
         this->UpdateMenus();
      }
   }
}

void AudacityProject::OnMacZoom()
{
   auto window = wxWindow::FindFocus();
   while (window && ! window->IsTopLevel())
      window = window->GetParent();
   if (window) {
      auto peer = window->GetPeer();
      peer->GetWXPeer();
      auto widget = static_cast<wxWidgetCocoaImpl*>(peer)->GetWXWidget();
      auto nsWindow = [widget window];
      if (nsWindow)
         [nsWindow performZoom:widget];
   }
}

void AudacityProject::OnMacBringAllToFront()
{
}
