//
//  wxPanelWrapper.cpp
//  Audacity
//
//  Created by Paul Licameli on 6/25/16.
//
//

#include "../Audacity.h"
#include "wxPanelWrapper.h"

#include <wx/grid.h>

void wxTabTraversalWrapperCharHook(wxKeyEvent &event)
{
#ifdef __WXMAC__
   // Compensate for the regressions in TAB key navigation
   // due to the switch to wxWidgets 3.0.2
   if (event.GetKeyCode() == WXK_TAB) {
      auto focus = wxWindow::FindFocus();
      if (dynamic_cast<wxGrid*>(focus)
         || (focus &&
             focus->GetParent() &&
             dynamic_cast<wxGrid*>(focus->GetParent()->GetParent()))) {
         // Let wxGrid do its own TAB key handling
         event.Skip();
         return;
      }
      focus->Navigate(
         event.ShiftDown()
         ? wxNavigationKeyEvent::IsBackward
         :  wxNavigationKeyEvent::IsForward
      );
      return;
   }
#endif

   event.Skip();
}
