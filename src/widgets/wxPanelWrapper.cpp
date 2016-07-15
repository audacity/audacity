//
//  wxPanelWrapper.cpp
//  Audacity
//
//  Created by Paul Licameli on 6/25/16.
//
//

#include "../Audacity.h"
#include "wxPanelWrapper.h"

void wxTabTraversalWrapperCharHook(wxKeyEvent &event)
{
#ifdef __WXMAC__
   // Compensate for the regressions in TAB key navigation
   // due to the switch to wxWidgets 3.0.2
   if (event.GetKeyCode() == WXK_TAB) {
      wxWindow::FindFocus()->Navigate(
         event.ShiftDown()
         ? wxNavigationKeyEvent::IsBackward
         :  wxNavigationKeyEvent::IsForward
      );
      return;
   }
#endif

   event.Skip();
}
