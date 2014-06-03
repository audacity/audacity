/**********************************************************************

  Audacity: A Digital Audio Editor

  HtmlWindow.cpp

  Leland Lucius

*******************************************************************//**

\file HtmlWindow.cpp

  Implements HtmlWindow

*//*******************************************************************//**

\class HtmlWindow
\brief The widget to the left of a ToolBar that allows it to be dragged
around to new positions.

*//**********************************************************************/

#ifndef __AUDACITY_WIDGETS_HtmlWindow__
#define __AUDACITY_WIDGETS_HtmlWindow__

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/html/htmlwin.h>

////////////////////////////////////////////////////////////
/// HtmlWindow Class
////////////////////////////////////////////////////////////

class AUDACITY_DLL_API HtmlWindow:public wxHtmlWindow
{
public:
   HtmlWindow(wxWindow *parent,
              wxWindowID id = wxID_ANY,
              const wxPoint& pos = wxDefaultPosition,
              const wxSize& size = wxDefaultSize,
              long style = wxHW_DEFAULT_STYLE,
              const wxString& name = wxT("htmlWindow"));
   virtual ~HtmlWindow();
};

#if wxUSE_ACCESSIBILITY

class HtmlWindowAx: public wxWindowAccessible
{
public:
   HtmlWindowAx(wxWindow * window);

   virtual ~ HtmlWindowAx();

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   virtual wxAccStatus GetChild( int childId, wxAccessible** child );

   // Gets the number of children.
   virtual wxAccStatus GetChildCount(int* childCount);

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   virtual wxAccStatus GetDefaultAction( int childId, wxString *actionName );

   // Returns the description for this object or a child.
   virtual wxAccStatus GetDescription( int childId, wxString *description );

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   virtual wxAccStatus GetFocus( int *childId, wxAccessible **child );

   // Returns help text for this object or a child, similar to tooltip text.
   virtual wxAccStatus GetHelpText( int childId, wxString *helpText );

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   virtual wxAccStatus GetKeyboardShortcut( int childId, wxString *shortcut );

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   virtual wxAccStatus GetLocation( wxRect& rect, int elementId );

   // Gets the name of the specified object.
   virtual wxAccStatus GetName( int childId, wxString *name );

   // Returns a role constant.
   virtual wxAccStatus GetRole( int childId, wxAccRole *role );

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   virtual wxAccStatus GetSelections( wxVariant *selections );

   // Returns a state constant.
   virtual wxAccStatus GetState(int childId, long* state);

   // Returns a localized string representing the value for the object
   // or child.
   virtual wxAccStatus GetValue(int childId, wxString* strValue);

};

#endif // wxUSE_ACCESSIBILITY

#endif
