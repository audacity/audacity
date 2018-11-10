/**********************************************************************

  Audacity: A Digital Audio Editor

  WindowAccessible.cpp

  David Bailes

*******************************************************************//**

\class WindowAccessible
\brief An alternative to using wxWindowAccessible, which in wxWidgets 3.1.1
contained GetParent() which was incorrect.

*//*******************************************************************/

#include "WindowAccessible.h"

#include <wx/window.h>

#if wxUSE_ACCESSIBILITY

WindowAccessible::WindowAccessible(wxWindow* win)
    : wxAccessible(win)
{
    if (win) win->SetAccessible(this);
}

wxAccStatus WindowAccessible::GetName(int childId, wxString* name)
{
   wxCHECK( GetWindow() != nullptr, wxACC_FAIL);

   // If the control has children, don't override their names
   if (childId > 0)
      return wxACC_NOT_IMPLEMENTED;

   *name = GetWindow()->GetName();
   return wxACC_OK;
}

#endif      // wxUSE_ACCESSIBILITY
