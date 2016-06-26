//
//  wxPanelWrapper.cpp
//  Audacity
//
//  Created by Paul Licameli on 6/25/16.
//
//

#include "../Audacity.h"
#include "wxPanelWrapper.h"

IMPLEMENT_CLASS(wxPanelWrapper, wxPanel)

wxPanelWrapper::wxPanelWrapper(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos,
                           const wxSize & size,
                           long style)
: wxPanel(parent, id, pos, size, style)
{}

void wxPanelWrapper::DoCharHook(wxKeyEvent &event)
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

void wxPanelWrapper::OnCharHook(wxKeyEvent &event)
{
   DoCharHook(event);
}

BEGIN_EVENT_TABLE(wxPanelWrapper, wxPanel)
   EVT_CHAR_HOOK(wxPanelWrapper::OnCharHook)
END_EVENT_TABLE()
