/**********************************************************************

  Audacity: A Digital Audio Editor

  Grid.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_GRID__
#define __AUDACITY_WIDGETS_GRID__

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/grid.h>
#include <wx/string.h>
#include <wx/window.h>

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>

class GridAx;

#endif

class TimeTextCtrl;

// ----------------------------------------------------------------------------
// TimeEditor
//
// wxGridCellEditor for the TimeTextCtrl.
// ----------------------------------------------------------------------------
#define GRID_VALUE_TIME wxT("Time")

class TimeEditor:public wxGridCellEditor
{
 public:

   TimeEditor();

   TimeEditor(const wxString &format, double rate);

   ~TimeEditor();

   void Create(wxWindow *parent, wxWindowID id, wxEvtHandler *handler);

   bool IsAcceptedKey(wxKeyEvent &event);

   void SetSize(const wxRect &rect);

   void BeginEdit(int row, int col, wxGrid *grid);
   bool EndEdit(int row, int col, wxGrid *grid);

   void Reset();

   wxString GetFormat();
   double GetRate();
   void SetFormat(const wxString &format);
   void SetRate(double rate);

   wxGridCellEditor *Clone() const;
   wxString GetValue() const;

   TimeTextCtrl *GetTimeCtrl() const { return (TimeTextCtrl *)m_control; };

 private:

   wxString mFormat;
   double mRate;
   double mOld;
};

// ----------------------------------------------------------------------------
// TimeRenderer
//
// wxGridCellRenderer for the TimeTextCtrl.
// ----------------------------------------------------------------------------

class TimeRenderer : public wxGridCellRenderer
{
 public:
    void Draw(wxGrid &grid,
              wxGridCellAttr &attr,
              wxDC &dc,
              const wxRect &rect,
              int row,
              int col,
              bool isSelected);

   wxSize GetBestSize(wxGrid &grid,
                      wxGridCellAttr &attr,
                      wxDC &dc,
                      int row,
                      int col);

   wxGridCellRenderer *Clone() const;
};

// ----------------------------------------------------------------------------
// ChoiceEditor
//
// Modified version of wxGridChoiceEditor using wxChoice instead of wxComboBox.
// ----------------------------------------------------------------------------
#define GRID_VALUE_CHOICE wxT("Choice")

class ChoiceEditor : public wxGridCellEditor, wxEvtHandler
{
public:

   ChoiceEditor(size_t count = 0,
                const wxString choices[] = NULL);

   ChoiceEditor(const wxArrayString &choices);

   ~ChoiceEditor();

   virtual void Create(wxWindow *parent,
                       wxWindowID id,
                       wxEvtHandler *evtHandler);

   void SetSize(const wxRect &rect);

   void BeginEdit(int row, int col, wxGrid *grid);
   bool EndEdit(int row, int col, wxGrid *grid);

   void Reset();

   wxGridCellEditor *Clone() const;

   void SetChoices(const wxArrayString &choices);
   wxString GetValue() const;

 protected:

   wxChoice *Choice() const { return (wxChoice *)m_control; }

 private:

   // A whole separate class just to get rid of Visual C++ warning C4407
   class FocusHandler:wxEvtHandler
   {
   public:
      void ConnectEvent(wxWindow *w)
      {
         w->GetEventHandler()->Connect(wxEVT_KILL_FOCUS, wxFocusEventHandler(FocusHandler::OnKillFocus));
      };
      void DisconnectEvent(wxWindow *w)
      {
         w->GetEventHandler()->Disconnect(wxEVT_KILL_FOCUS, wxFocusEventHandler(FocusHandler::OnKillFocus));
      };
      void OnKillFocus(wxFocusEvent & WXUNUSED(event))
      {
         return;
      };
   } mHandler;

   wxArrayString mChoices;
   wxString mOld;
};

// ----------------------------------------------------------------------------
// Grid
//
// wxGrid with support for accessibility.
// ----------------------------------------------------------------------------

class Grid
: public wxGrid
{

 public:

   Grid(wxWindow *parent,
        wxWindowID id,
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxWANTS_CHARS | wxBORDER,
        const wxString& name = wxPanelNameStr);

   ~Grid();

#if wxUSE_ACCESSIBILITY
   void ClearGrid();
   bool InsertRows(int pos = 0, int numRows = 1, bool updateLabels = true);
   bool AppendRows(int numRows = 1, bool updateLabels = true);
   bool DeleteRows(int pos = 0, int numRows = 1, bool updateLabels = true);
   bool InsertCols(int pos = 0, int numCols = 1, bool updateLabels = true);
   bool AppendCols(int numCols = 1, bool updateLabels = true);
   bool DeleteCols(int pos = 0, int numCols = 1, bool updateLabels = true);

   GridAx *GetNextAx(GridAx *parent, wxAccRole role, int row, int col);
#endif

 protected:

   void OnSetFocus(wxFocusEvent &event);
   void OnSelectCell(wxGridEvent &event);
   void OnKeyDown(wxKeyEvent &event);

 private:

#if wxUSE_ACCESSIBILITY
   GridAx *mAx;
   wxArrayPtrVoid mChildren;
   int mObjNdx;
#endif

 public:

   DECLARE_EVENT_TABLE();
};

#if wxUSE_ACCESSIBILITY
// ----------------------------------------------------------------------------
// GridAx
//
// wxAccessible object providing grid information for Grid.
// ----------------------------------------------------------------------------

class GridAx
: public wxWindowAccessible
{

 public:

   GridAx(Grid *grid);

   void SetCurrentCell(int row, int col);
   void TableUpdated();
   bool GetRowCol(int childId, int & row, int & col);

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   virtual wxAccStatus GetChild(int childId, wxAccessible **child);

   // Gets the number of children.
   virtual wxAccStatus GetChildCount(int *childCount);

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   virtual wxAccStatus GetDefaultAction(int childId, wxString *actionName);

   // Returns the description for this object or a child.
   virtual wxAccStatus GetDescription(int childId, wxString *description);

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   virtual wxAccStatus GetFocus(int *childId, wxAccessible **child);

   // Returns help text for this object or a child, similar to tooltip text.
   virtual wxAccStatus GetHelpText(int childId, wxString *helpText);

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   virtual wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut);

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   virtual wxAccStatus GetLocation(wxRect & rect, int elementId);

   // Gets the name of the specified object.
   virtual wxAccStatus GetName(int childId, wxString *name);

   // Gets the parent, or NULL.
   virtual wxAccStatus GetParent(wxAccessible **parent);

   // Returns a role constant.
   virtual wxAccStatus GetRole(int childId, wxAccRole *role);

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

#if defined(__WXMAC__)
   // Selects the object or child.
   virtual wxAccStatus Select(int childId, wxAccSelectionFlags selectFlags);
#endif

   Grid *mGrid;
   int mLastId;

};
#endif

#endif

