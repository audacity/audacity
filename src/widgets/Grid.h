/**********************************************************************

  Audacity: A Digital Audio Editor

  Grid.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_GRID__
#define __AUDACITY_WIDGETS_GRID__

#include "../MemoryX.h"
#include <vector>
#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/grid.h>
#include <wx/string.h>
#include <wx/window.h>
#include "NumericTextCtrl.h"

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>

class GridAx;

#endif

class NumericTextCtrl;

// ----------------------------------------------------------------------------
// NumericEditor
//
// wxGridCellEditor for the NumericTextCtrl.
// ----------------------------------------------------------------------------
#define GRID_VALUE_TIME wxT("Time")
#define GRID_VALUE_FREQUENCY wxT("Frequency")

class NumericEditor /* not final */ : public wxGridCellEditor
{
public:

   NumericEditor
      (NumericConverter::Type type, const wxString &format, double rate);

   ~NumericEditor();

   // Precondition: parent != NULL
   void Create(wxWindow *parent, wxWindowID id, wxEvtHandler *handler) override;

   bool IsAcceptedKey(wxKeyEvent &event) override;

   void SetSize(const wxRect &rect) override;

   void BeginEdit(int row, int col, wxGrid *grid) override;

   bool EndEdit(int row, int col, const wxGrid *grid, const wxString &oldval, wxString *newval) override;

   void ApplyEdit(int row, int col, wxGrid *grid) override;

   void Reset() override;

   wxString GetFormat() const;
   double GetRate() const;
   void SetFormat(const wxString &format);
   void SetRate(double rate);

   wxGridCellEditor *Clone() const override;
   wxString GetValue() const override;

   NumericTextCtrl *GetNumericTextControl() const
      { return static_cast<NumericTextCtrl *>(m_control); }

 private:

   wxString mFormat;
   double mRate;
   NumericConverter::Type mType;
   double mOld;
   wxString mOldString;
   wxString mValueAsString;
};

// ----------------------------------------------------------------------------
// NumericRenderer
//
// wxGridCellRenderer for the NumericTextCtrl.
// ----------------------------------------------------------------------------

class NumericRenderer final : public wxGridCellRenderer
{
 public:
   NumericRenderer(NumericConverter::Type type) : mType{ type } {}
   ~NumericRenderer() override;

   void Draw(wxGrid &grid,
              wxGridCellAttr &attr,
              wxDC &dc,
              const wxRect &rect,
              int row,
              int col,
              bool isSelected) override;

   wxSize GetBestSize(wxGrid &grid,
                      wxGridCellAttr &attr,
                      wxDC &dc,
                      int row,
                      int col) override;

   wxGridCellRenderer *Clone() const override;

private:
   NumericConverter::Type mType;
};

// ----------------------------------------------------------------------------
// ChoiceEditor
//
// Modified version of wxGridChoiceEditor using wxChoice instead of wxComboBox.
// ----------------------------------------------------------------------------
#define GRID_VALUE_CHOICE wxT("Choice")

class ChoiceEditor final : public wxGridCellEditor, wxEvtHandler
{
public:

   ChoiceEditor(size_t count = 0,
                const wxString choices[] = NULL);

   ChoiceEditor(const wxArrayString &choices);

   ~ChoiceEditor();

   void Create(wxWindow *parent,
                       wxWindowID id,
                       wxEvtHandler *evtHandler) override;

   void SetSize(const wxRect &rect);

   void BeginEdit(int row, int col, wxGrid *grid);

   bool EndEdit(int row, int col, wxGrid *grid);

   bool EndEdit(int row, int col, const wxGrid *grid, const wxString &oldval, wxString *newval);

   void ApplyEdit(int row, int col, wxGrid *grid);

   void Reset();

   wxGridCellEditor *Clone() const override;

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
   wxString mValueAsString;
};

// ----------------------------------------------------------------------------
// Grid
//
// wxGrid with support for accessibility.
// ----------------------------------------------------------------------------

class Grid final : public wxGrid
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
   std::vector<movable_ptr<GridAx>> mChildren;
   int mObjNdx;
#endif

 public:

   DECLARE_EVENT_TABLE()
};

#if wxUSE_ACCESSIBILITY
// ----------------------------------------------------------------------------
// GridAx
//
// wxAccessible object providing grid information for Grid.
// ----------------------------------------------------------------------------

class GridAx final : public wxWindowAccessible
{

 public:

   GridAx(Grid *grid);

   void SetCurrentCell(int row, int col);
   void TableUpdated();
   bool GetRowCol(int childId, int & row, int & col);

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
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

#if defined(__WXMAC__)
   // Selects the object or child.
   wxAccStatus Select(int childId, wxAccSelectionFlags selectFlags) override;
#endif

   Grid *mGrid;
   int mLastId;

};
#endif

#endif

