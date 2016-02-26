/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelAx.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_ACCESSIBILITY__
#define __AUDACITY_TRACK_PANEL_ACCESSIBILITY__

#include <wx/window.h>
#include <wx/panel.h>

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>
#endif

#include "TrackPanel.h"

class TrackPanelAx final
#if wxUSE_ACCESSIBILITY
   : public wxWindowAccessible
#endif
{
public:
   TrackPanelAx(wxWindow * window);
   virtual ~ TrackPanelAx();

   // Returns currently focused track or first one if none focused
   Track *GetFocus();

   // Changes focus to a specified track
   void SetFocus( Track *track );

   // Returns TRUE if passed track has the focus
   bool IsFocused( Track *track );

   // Called to signal changes to a track
   void Updated();

#if wxUSE_ACCESSIBILITY
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
#endif

private:

   int TrackNum( Track *track );
   Track *FindTrack( int num );

   TrackPanel *mTrackPanel;
   Track *mFocusedTrack;
};

#endif // __AUDACITY_TRACK_PANEL_ACCESSIBILITY__
