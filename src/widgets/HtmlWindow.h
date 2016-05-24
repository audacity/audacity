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
around to NEW positions.

*//**********************************************************************/

#ifndef __AUDACITY_WIDGETS_HtmlWindow__
#define __AUDACITY_WIDGETS_HtmlWindow__

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/html/htmlwin.h>

////////////////////////////////////////////////////////////
/// HtmlWindow Class
////////////////////////////////////////////////////////////

class AUDACITY_DLL_API HtmlWindow /* not final */ : public wxHtmlWindow
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

class HtmlWindowAx final : public wxWindowAccessible
{
public:
   HtmlWindowAx(wxWindow * window);

   virtual ~ HtmlWindowAx();

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible** child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int* childCount) override;

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription(int childId, wxString *description) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus(int *childId, wxAccessible **child) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int childId, wxString *helpText) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation(wxRect& rect, int elementId) override;

   // Gets the name of the specified object.
   wxAccStatus GetName(int childId, wxString *name) override;

   // Returns a role constant.
   wxAccStatus GetRole(int childId, wxAccRole *role) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   wxAccStatus GetSelections(wxVariant *selections) override;

   // Returns a state constant.
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

};

#endif // wxUSE_ACCESSIBILITY

#endif
