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

#include <wx/setup.h> // for wxUSE_* macros
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

#include <wx/slider.h>

SliderAx::SliderAx(wxWindow * window, const TranslatableString &fmt) :
WindowAccessible( window )
{
   mParent = window;
   mFmt = fmt;
}

SliderAx::~SliderAx()
{
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus SliderAx::GetChild( int childId, wxAccessible** child )
{
   if( childId == wxACC_SELF )
   {
      *child = this;
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus SliderAx::GetChildCount(int* childCount)
{
   *childCount = 3;

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus SliderAx::GetDefaultAction( int WXUNUSED(childId), wxString *actionName )
{
   actionName->clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus SliderAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus SliderAx::GetFocus(int* childId, wxAccessible** child)
{
   *childId = 0;
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus SliderAx::GetHelpText( int WXUNUSED(childId), wxString *helpText )
{
   helpText->clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus SliderAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus SliderAx::GetLocation( wxRect& rect, int WXUNUSED(elementId) )
{
   wxSlider *s = wxDynamicCast( GetWindow(), wxSlider );

   rect = s->GetRect();
   rect.SetPosition( s->GetParent()->ClientToScreen( rect.GetPosition() ) );

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus SliderAx::GetName(int WXUNUSED(childId), wxString* name)
{
   wxSlider *s = wxDynamicCast( GetWindow(), wxSlider );

   *name = s->GetName();

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus SliderAx::GetRole(int childId, wxAccRole* role)
{
   switch( childId )
   {
   case 0:
      *role = wxROLE_SYSTEM_SLIDER;
      break;

   case 1:
   case 3:
      *role = wxROLE_SYSTEM_PUSHBUTTON;
      break;

   case 2:
      *role = wxROLE_SYSTEM_INDICATOR;
      break;
   }

   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus SliderAx::GetSelections( wxVariant * WXUNUSED(selections) )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus SliderAx::GetState(int childId, long* state)
{
   wxSlider *s = wxDynamicCast( GetWindow(), wxSlider );

   switch( childId )
   {
   case 0:
      *state = wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

   case 1:
      if( s->GetValue() == s->GetMin() )
      {
         *state = wxACC_STATE_SYSTEM_INVISIBLE;
      }
      break;

   case 3:
      if( s->GetValue() == s->GetMax() )
      {
         *state = wxACC_STATE_SYSTEM_INVISIBLE;
      }
      break;
   }

   // Do not use mSliderIsFocused is not set until after this method
   // is called.
   *state |= ( s == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0 );

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus SliderAx::GetValue(int childId, wxString* strValue)
{
   wxSlider *s = wxDynamicCast( GetWindow(), wxSlider );

   if( childId == 0 )
   {
      strValue->Printf( mFmt.Translation(), s->GetValue() );

      return wxACC_OK;
   }

   return wxACC_NOT_SUPPORTED;
}

#endif      // wxUSE_ACCESSIBILITY
