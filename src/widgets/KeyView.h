/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyView.h

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_KEYVIEW__
#define __AUDACITY_WIDGETS_KEYVIEW__

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/arrstr.h>
#include <wx/dynarray.h>
#include <wx/string.h>
#include <wx/vlbox.h>

// Class holding all information about a node.  Rather than a real tree
// we store these in an array and simulate a tree.
class KeyNode
{
public:
   KeyNode()
   {
      index = -1;
      line = -1;
      depth = -1;
      iscat = false;
      ispfx = false;
      isparent = false;
      isopen = false;
   }

public:
   wxString name;
   wxString category;
   wxString prefix;
   wxString label;
   wxString key;
   int index;
   int line;
   int depth;
   bool iscat;
   bool ispfx;
   bool isparent;
   bool isopen;
};

// Declare the KeyNode arrays
WX_DECLARE_OBJARRAY(KeyNode, KeyNodeArray);
WX_DECLARE_OBJARRAY(KeyNode *, KeyNodeArrayPtr);

// Types of view currently supported
enum ViewByType
{
   ViewByTree,
   ViewByName,
   ViewByKey
};

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>

// Forward reference accessibility provideer
class KeyViewAx;
#endif

// The KeyView class
class KeyView final : public wxVListBox
{
public:
   KeyView(wxWindow *parent,
           wxWindowID id = wxID_ANY,
           const wxPoint & pos = wxDefaultPosition,
           const wxSize & size = wxDefaultSize);
   virtual ~KeyView();
   wxString GetName() const; // Gets the control name from the base class

   void RefreshBindings(const wxArrayString & names,
                        const wxArrayString & categories,
                        const wxArrayString & prefixes,
                        const wxArrayString & labels,
                        const wxArrayString & keys);

   int GetSelected() const;

   wxString GetLabel(int index) const;
   wxString GetFullLabel(int index) const;

   int GetIndexByName(const wxString & name) const;
   wxString GetName(int index) const;
   wxString GetNameByKey(const wxString & key) const;

   int GetIndexByKey(const wxString & key) const;
   wxString GetKey(int index) const;
   bool CanSetKey(int index) const;
   bool SetKey(int index, const wxString & key);
   bool SetKeyByName(const wxString & name, const wxString & key);

   void SetView(ViewByType type);

   void SetFilter(const wxString & filter);

   void ExpandAll();
   void CollapseAll();

private:
   void RecalcExtents();
   void UpdateHScroll();
   void RefreshLines();

   void SelectNode(int index);

   int LineToIndex(int line) const;
   int IndexToLine(int index) const;

   void OnDrawBackground(wxDC & dc, const wxRect & rect, size_t line) const;
   void OnDrawItem(wxDC & dc, const wxRect & rect, size_t line) const;
   wxCoord OnMeasureItem(size_t line) const;

   void OnSelected(wxCommandEvent & event);
   void OnSetFocus(wxFocusEvent & event);
   void OnKillFocus(wxFocusEvent & event);
   void OnSize(wxSizeEvent & event);
   void OnScroll(wxScrollWinEvent & event);
   void OnKeyDown(wxKeyEvent & event);
   void OnLeftDown(wxMouseEvent & event);

   static int CmpKeyNodeByTree(KeyNode ***n1, KeyNode ***n2);
   static int CmpKeyNodeByName(KeyNode ***n1, KeyNode ***n2);
   static int CmpKeyNodeByKey(KeyNode ***n1, KeyNode ***n2);

#if wxUSE_ACCESSIBILITY
   friend class KeyViewAx;

   bool HasChildren(int line);
   bool IsExpanded(int line);
   wxCoord GetLineHeight(int line);
   wxString GetValue(int line);
   ViewByType GetViewType();
#endif

private:
   KeyNodeArray mNodes;
   KeyNodeArrayPtr mLines;

   ViewByType mViewType;
   wxString mFilter;

   wxCoord mScrollX;
   wxCoord mWidth;

   size_t mLineCount;
   wxCoord mLineHeight;
   wxCoord mKeyX;
   int mCommandWidth;
   wxCoord mKeyWidth;

#if wxUSE_ACCESSIBILITY
   KeyViewAx *mAx;
#endif

   DECLARE_EVENT_TABLE()
};

#if wxUSE_ACCESSIBILITY

// ----------------------------------------------------------------------------
// KeyViewAx
//
// wxAccessible object providing information for KeyView.
// ----------------------------------------------------------------------------

class KeyViewAx final : public wxWindowAccessible
{
public:

   KeyViewAx(KeyView *view);

   void SetCurrentLine(int row);
   void ListUpdated();
   bool LineToId(int line, int & childId);
   bool IdToLine(int childId, int & line);

   // Can return either a child object, or an integer
   // representing the child element, starting from 1.
   wxAccStatus HitTest(const wxPoint & pt, int *childId, wxAccessible **childObject) override;

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible **child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int *childCount) override;

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
   wxAccStatus GetLocation(wxRect & rect, int elementId) override;

   // Navigates from fromId to toId/toObject.
   wxAccStatus Navigate(wxNavDir navDir, int fromId,
                                int *toId, wxAccessible **toObject) override;

   // Gets the name of the specified object.
   wxAccStatus GetName(int childId, wxString *name) override;

   // Gets the parent, or NULL.
   wxAccStatus GetParent(wxAccessible **parent) override;

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
   wxAccStatus GetState(int childId, long *state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString *strValue) override;

#if defined(__WXMAC__)
   // Selects the object or child.
   wxAccStatus Select(int childId, wxAccSelectionFlags selectFlags) override;
#endif

private:
   KeyView *mView;
   int mLastId;
};

#endif

#endif

