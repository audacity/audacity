/**********************************************************************

  Audacity: A Digital Audio Editor

  Grid.cpp

  Leland Lucius

*******************************************************************//**

\class Grid
\brief Supplies an accessible grid based on wxGrid.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dc.h>
#include <wx/grid.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/toplevel.h>

#include "Grid.h"
#include "TimeTextCtrl.h"

TimeEditor::TimeEditor()
{
   TimeEditor(wxT("seconds"), 44100);
}

TimeEditor::TimeEditor(const wxString &format, double rate)
{
   mFormat = format;
   mRate = rate;
   mOld = 0.0;
}

TimeEditor::~TimeEditor()
{
}

void TimeEditor::Create(wxWindow *parent, wxWindowID id, wxEvtHandler *handler)
{
   m_control = new TimeTextCtrl(parent,
                                wxID_ANY,
                                mFormat,
                                mOld,
                                mRate,
                                wxDefaultPosition,
                                wxDefaultSize,
                                true);

   wxGridCellEditor::Create(parent, id, handler);
}

void TimeEditor::SetSize(const wxRect &rect)
{
   wxSize size = m_control->GetSize();

   // Always center...looks bad otherwise
   int x = rect.x + ((rect.width / 2) - (size.x / 2)) + 1;
   int y = rect.y + ((rect.height / 2) - (size.y / 2)) + 1;

   m_control->Move(x, y);
}

void TimeEditor::BeginEdit(int row, int col, wxGrid *grid)
{
   wxGridTableBase *table = grid->GetTable();

   table->GetValue(row, col).ToDouble(&mOld);

   GetTimeCtrl()->SetTimeValue(mOld);
   GetTimeCtrl()->EnableMenu();

   GetTimeCtrl()->SetFocus();
}

bool TimeEditor::EndEdit(int row, int col, wxGrid *grid)
{
   double newtime = GetTimeCtrl()->GetTimeValue();
   bool changed = newtime != mOld;

   if (changed) {
      grid->GetTable()->SetValue(row, col, wxString::Format(wxT("%g"), newtime));
   }

   return changed;
}

void TimeEditor::Reset()
{
   GetTimeCtrl()->SetTimeValue(mOld);
}

bool TimeEditor::IsAcceptedKey(wxKeyEvent &event)
{
   if (wxGridCellEditor::IsAcceptedKey(event)) {
      if (event.GetKeyCode() == WXK_RETURN) {
         return true;
      }
   }

   return false;
}

wxGridCellEditor *TimeEditor::Clone() const
{
   return new TimeEditor(mFormat, mRate);
}

wxString TimeEditor::GetValue() const
{
   return wxString::Format(wxT("%g"), GetTimeCtrl()->GetTimeValue());
}

wxString TimeEditor::GetFormat()
{
   return mFormat;
}

double TimeEditor::GetRate()
{
   return mRate;
}

void TimeEditor::SetFormat(const wxString &format)
{
   mFormat = format;
}

void TimeEditor::SetRate(double rate)
{
   mRate = rate;
}

void TimeRenderer::Draw(wxGrid &grid,
                        wxGridCellAttr &attr,
                        wxDC &dc,
                        const wxRect &rect,
                        int row,
                        int col,
                        bool isSelected)
{
   wxGridCellRenderer::Draw(grid, attr, dc, rect, row, col, isSelected);

   wxGridTableBase *table = grid.GetTable();
   TimeEditor *te = (TimeEditor *) grid.GetCellEditor(row, col);
   wxString tstr;

   if (te) {
      double value;

      table->GetValue(row, col).ToDouble(&value);

      TimeTextCtrl tt(&grid,
                      wxID_ANY,
                      te->GetFormat(),
                      value,
                      te->GetRate(),
                      wxPoint(10000, 10000),  // create offscreen
                      wxDefaultSize,
                      true);
      tstr = tt.GetTimeString();

      te->DecRef();
   }

   dc.SetBackgroundMode(wxTRANSPARENT);

   if (grid.IsEnabled())
   {
      if (isSelected)
      {
         dc.SetTextBackground(grid.GetSelectionBackground());
         dc.SetTextForeground(grid.GetSelectionForeground());
      }
      else
      {
         dc.SetTextBackground(attr.GetBackgroundColour());
         dc.SetTextForeground(attr.GetTextColour());
      }
   }
   else
   {
      dc.SetTextBackground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
      dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_GRAYTEXT));
   }

   dc.SetFont(attr.GetFont());

   int hAlign, vAlign;

   attr.GetAlignment(&hAlign, &vAlign);

   grid.DrawTextRectangle(dc, tstr, rect, hAlign, vAlign);
}

wxSize TimeRenderer::GetBestSize(wxGrid &grid,
                                 wxGridCellAttr & WXUNUSED(attr),
                                 wxDC & WXUNUSED(dc),
                                 int row,
                                 int col)
{
   wxGridTableBase *table = grid.GetTable();
   TimeEditor *te = (TimeEditor *) grid.GetCellEditor(row, col);
   wxSize sz;

   if (te) {
      double value;
      table->GetValue(row, col).ToDouble(&value);
      TimeTextCtrl tt(&grid,
                      wxID_ANY,
                      te->GetFormat(),
                      value,
                      te->GetRate(),
                      wxPoint(10000, 10000),  // create offscreen
                      wxDefaultSize,
                      true);
      sz = tt.GetSize();

      te->DecRef();
   }

   return sz;
}

wxGridCellRenderer *TimeRenderer::Clone() const
{
   return new TimeRenderer();
}

ChoiceEditor::ChoiceEditor(size_t count, const wxString choices[])
{
   if (count) {
      mChoices.Alloc(count);
      for (size_t n = 0; n < count; n++) {
         mChoices.Add(choices[n]);
      }
   }
}

ChoiceEditor::ChoiceEditor(const wxArrayString &choices)
{
   mChoices = choices;
}

ChoiceEditor::~ChoiceEditor()
{
   if (m_control)
      mHandler.DisconnectEvent(m_control);
}

wxGridCellEditor *ChoiceEditor::Clone() const
{
   return new ChoiceEditor(mChoices);
}

void ChoiceEditor::Create(wxWindow* parent, wxWindowID id, wxEvtHandler* evtHandler)
{
   m_control = new wxChoice(parent,
                            id,
                            wxDefaultPosition,
                            wxDefaultSize,
                            mChoices);

   wxGridCellEditor::Create(parent, id, evtHandler);
   mHandler.ConnectEvent(m_control);
}

void ChoiceEditor::SetSize(const wxRect &rect)
{
   wxSize size = m_control->GetSize();

   // Always center...looks bad otherwise
   int x = rect.x + ((rect.width / 2) - (size.x / 2)) + 1;
   int y = rect.y + ((rect.height / 2) - (size.y / 2)) + 1;

   m_control->Move(x, y);
}

void ChoiceEditor::BeginEdit(int row, int col, wxGrid* grid)
{
   if (!m_control)
      return;

   mOld = grid->GetTable()->GetValue(row, col);

   Choice()->Clear();
   Choice()->Append(mChoices);
   Choice()->SetSelection(mChoices.Index(mOld));
   Choice()->SetFocus();
}

bool ChoiceEditor::EndEdit(int row, int col,
                           wxGrid* grid)
{
   int sel = Choice()->GetSelection();

   // This can happen if the wxChoice control is displayed and the list of choices get changed
   if ((sel < 0) || (sel >= (int)(mChoices.GetCount())))
   {
      return false;
   }

   wxString val = mChoices[sel];
   if (val == mOld)
      return false;

   grid->GetTable()->SetValue(row, col, val);

   return true;
}

void ChoiceEditor::Reset()
{
   Choice()->SetSelection(mChoices.Index(mOld));
}

void ChoiceEditor::SetChoices(const wxArrayString &choices)
{
   mChoices = choices;
}

wxString ChoiceEditor::GetValue() const
{
   return mChoices[Choice()->GetSelection()];
}

///
///
///

BEGIN_EVENT_TABLE(Grid, wxGrid)
   EVT_SET_FOCUS(Grid::OnSetFocus)
   EVT_KEY_DOWN(Grid::OnKeyDown)
   EVT_GRID_SELECT_CELL(Grid::OnSelectCell)
END_EVENT_TABLE()

Grid::Grid(wxWindow *parent,
           wxWindowID id,
           const wxPoint& pos,
           const wxSize& size,
           long style,
           const wxString& name)
: wxGrid(parent, id, pos, size, style | wxWANTS_CHARS, name)
{
#if wxUSE_ACCESSIBILITY
   mAx = new GridAx(this);
   GetGridWindow()->SetAccessible(mAx);
#endif

   RegisterDataType(GRID_VALUE_TIME,
                    new TimeRenderer,
                    new TimeEditor);

   RegisterDataType(GRID_VALUE_CHOICE,
                    new wxGridCellStringRenderer,
                    new ChoiceEditor);
}

Grid::~Grid()
{
#if wxUSE_ACCESSIBILITY
   int cnt = mChildren.GetCount();

   while (cnt) {
      GridAx *ax = (GridAx *) mChildren[--cnt];
      delete ax;
   }
#endif
}

void Grid::OnSetFocus(wxFocusEvent &event)
{
   event.Skip();

#if wxUSE_ACCESSIBILITY
   mAx->SetCurrentCell(GetGridCursorRow(), GetGridCursorCol());
#endif
}

void Grid::OnSelectCell(wxGridEvent &event)
{
   event.Skip();

#if wxUSE_ACCESSIBILITY
   mAx->SetCurrentCell(event.GetRow(), event.GetCol());
#endif
}

void Grid::OnKeyDown(wxKeyEvent &event)
{
   switch (event.GetKeyCode())
   {
      case WXK_LEFT:
      case WXK_RIGHT:
      {
         int rows = GetNumberRows();
         int cols = GetNumberCols();
         int crow = GetGridCursorRow();
         int ccol = GetGridCursorCol();

         if (event.GetKeyCode() == WXK_LEFT) {
            if (crow == 0 && ccol == 0) {
               // do nothing
            }
            else if (ccol == 0) {
               SetGridCursor(crow - 1, cols - 1);
            }
            else {
               SetGridCursor(crow, ccol - 1);
            }
         }
         else {
            if (crow == rows - 1 && ccol == cols - 1) {
               // do nothing
            }
            else if (ccol == cols - 1) {
               SetGridCursor(crow + 1, 0);
            }
            else {
               SetGridCursor(crow, ccol + 1);
            }
         }

#if wxUSE_ACCESSIBILITY
         // Make sure the new cell is made available to the screen reader
         mAx->SetCurrentCell(GetGridCursorRow(), GetGridCursorCol());
#endif
      }
      break;

      case WXK_TAB:
      {
         int rows = GetNumberRows();
         int cols = GetNumberCols();
         int crow = GetGridCursorRow();
         int ccol = GetGridCursorCol();

         if (event.ControlDown()) {
            int flags = wxNavigationKeyEvent::FromTab |
                        ( event.ShiftDown() ?
                          wxNavigationKeyEvent::IsBackward :
                          wxNavigationKeyEvent::IsForward );
            Navigate(flags);
            return;
         }
         else if (event.ShiftDown()) {
            if (crow == 0 && ccol == 0) {
               Navigate(wxNavigationKeyEvent::FromTab | wxNavigationKeyEvent::IsBackward);
               return;
            }
            else if (ccol == 0) {
               SetGridCursor(crow - 1, cols - 1);
            }
            else {
               SetGridCursor(crow, ccol - 1);
            }
         }
         else {
            if (crow == rows - 1 && ccol == cols - 1) {
               Navigate(wxNavigationKeyEvent::FromTab | wxNavigationKeyEvent::IsForward);
               return;
            }
            else if (ccol == cols - 1) {
               SetGridCursor(crow + 1, 0);
            }
            else {
               SetGridCursor(crow, ccol + 1);
            }
         }
         MakeCellVisible(GetGridCursorRow(), GetGridCursorCol());

#if wxUSE_ACCESSIBILITY
         // Make sure the new cell is made available to the screen reader
         mAx->SetCurrentCell(GetGridCursorRow(), GetGridCursorCol());
#endif
      }
      break;

      case WXK_RETURN:
      case WXK_NUMPAD_ENTER:
      {
         if (!IsCellEditControlShown()) {
            wxTopLevelWindow *tlw = wxDynamicCast(wxGetTopLevelParent(this), wxTopLevelWindow);
            wxWindow *def = tlw->GetDefaultItem();
            if (def && def->IsEnabled()) {
               wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                                     def->GetId());
               GetParent()->ProcessEvent(cevent);
            }
         }
         else {
            wxGrid::OnKeyDown(event);

            // This looks strange, but what it does is selects the cell when
            // enter is pressed after editing.  Without it, Jaws and Window-Eyes
            // do not speak the new cell contents (the one below the edited one).
            SetGridCursor(GetGridCursorRow(), GetGridCursorCol());
         }
         break;
      }

      default:
         wxGrid::OnKeyDown(event);
      break;
   }
}

#if wxUSE_ACCESSIBILITY
void Grid::ClearGrid()
{
   wxGrid::ClearGrid();

   mAx->TableUpdated();

   return;
}

bool Grid::InsertRows(int pos, int numRows, bool updateLabels)
{
   bool res = wxGrid::InsertRows(pos, numRows, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::AppendRows(int numRows, bool updateLabels)
{
   bool res = wxGrid::AppendRows(numRows, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::DeleteRows(int pos, int numRows, bool updateLabels)
{
   bool res = wxGrid::DeleteRows(pos, numRows, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::InsertCols(int pos, int numCols, bool updateLabels)
{
   bool res = wxGrid::InsertCols(pos, numCols, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::AppendCols(int numCols, bool updateLabels)
{
   bool res = wxGrid::AppendCols(numCols, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::DeleteCols(int pos, int numCols, bool updateLabels)
{
   bool res = wxGrid::DeleteCols(pos, numCols, updateLabels);

   mAx->TableUpdated();

   return res;
}

GridAx::GridAx(Grid *grid)
: wxWindowAccessible(grid->GetGridWindow())
{
   mGrid = grid;
   mLastId = -1;
}

void GridAx::TableUpdated()
{
   NotifyEvent(wxACC_EVENT_OBJECT_REORDER,
               mGrid->GetGridWindow(),
               wxOBJID_CLIENT,
               0);
}

void GridAx::SetCurrentCell(int row, int col)
{
   int id = (((row * mGrid->GetNumberCols()) + col) + 1);

   if (mLastId != -1) {
      NotifyEvent(wxACC_EVENT_OBJECT_SELECTIONREMOVE,
               mGrid->GetGridWindow(),
               wxOBJID_CLIENT,
               mLastId);
   }

   NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
               mGrid->GetGridWindow(),
               wxOBJID_CLIENT,
               id);

   NotifyEvent(wxACC_EVENT_OBJECT_SELECTION,
               mGrid->GetGridWindow(),
               wxOBJID_CLIENT,
               id);

   mLastId = id;
}

bool GridAx::GetRowCol(int childId, int & row, int & col)
{
   if (childId == wxACC_SELF) {
      return false;
   }

   int cols = mGrid->GetNumberCols();
   int id = childId - 1;

   row = id / cols;
   col = id % cols;

   return true;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus GridAx::GetChild(int childId, wxAccessible** child)
{
   if (childId == wxACC_SELF) {
      *child = this;
   }
   else {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus GridAx::GetChildCount(int *childCount)
{
   *childCount = mGrid->GetNumberRows() * mGrid->GetNumberCols();

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus GridAx::GetDefaultAction(int WXUNUSED(childId), wxString *actionName)
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus GridAx::GetDescription(int WXUNUSED(childId), wxString *description)
{
   description->Clear();

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus GridAx::GetHelpText(int WXUNUSED(childId), wxString *helpText)
{
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus GridAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString *shortcut)
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus GridAx::GetLocation(wxRect & rect, int elementId)
{
   wxRect r;
   int row;
   int col;

   if (GetRowCol(elementId, row, col)) {
      rect = mGrid->CellToRect(row, col);
      rect.SetPosition(mGrid->GetGridWindow()->ClientToScreen(rect.GetPosition()));
   }
   else {
      rect = mGrid->GetRect();
      rect.SetPosition(mGrid->GetParent()->ClientToScreen(rect.GetPosition()));
   }

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus GridAx::GetName(int childId, wxString *name)
{
   int row;
   int col;

   if (GetRowCol(childId, row, col)) {
      wxString n = mGrid->GetColLabelValue(col);
      wxString v = mGrid->GetCellValue(row, col);
      if (v.IsEmpty()) {
         v = _("Empty");
      }

      // Hack to provide a more intelligible response
      TimeEditor *d =
         (TimeEditor *)mGrid->GetDefaultEditorForType(GRID_VALUE_TIME);
      TimeEditor *c =
         (TimeEditor *)mGrid->GetCellEditor(row, col);

      if (c && d && c == d) {
         double value;
         v.ToDouble(&value);

         TimeTextCtrl tt(mGrid,
                         wxID_ANY,
                         c->GetFormat(),
                         value,
                         c->GetRate(),
                         wxPoint(10000, 10000),  // create offscreen
                         wxDefaultSize,
                         true);
         v = tt.GetTimeString();
      }

      if (c)
         c->DecRef();
      if (d)
         d->DecRef();

      *name = n + wxT(" ") + v;
   }

   return wxACC_OK;
}

wxAccStatus GridAx::GetParent(wxAccessible ** WXUNUSED(parent))
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a role constant.
wxAccStatus GridAx::GetRole(int childId, wxAccRole *role)
{
   if (childId == wxACC_SELF) {
#if defined(__WXMSW__)
      *role = wxROLE_SYSTEM_TABLE;
#endif

#if defined(__WXMAC__)
      *role = wxROLE_SYSTEM_GROUPING;
#endif
   }
   else {
      *role = wxROLE_SYSTEM_TEXT;
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
wxAccStatus GridAx::GetSelections(wxVariant * WXUNUSED(selections))
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus GridAx::GetState(int childId, long *state)
{
   int flag = wxACC_STATE_SYSTEM_FOCUSABLE |
              wxACC_STATE_SYSTEM_SELECTABLE;
   int col;
   int row;

   if (!GetRowCol(childId, row, col)) {
      *state = 0;
      return wxACC_FAIL;
   }

#if defined(__WXMSW__)
   flag |= wxACC_STATE_SYSTEM_FOCUSED |
           wxACC_STATE_SYSTEM_SELECTED;

      if (mGrid->IsReadOnly(row, col)) {
         flag = wxACC_STATE_SYSTEM_UNAVAILABLE;
      }
#endif

#if defined(__WXMAC__)
   if (mGrid->IsInSelection(row, col)) {
      flag |= wxACC_STATE_SYSTEM_SELECTED;
   }

   if (mGrid->GetGridCursorRow() == row && mGrid->GetGridCursorCol() == col) {
       flag |= wxACC_STATE_SYSTEM_FOCUSED;
   }

   if (mGrid->IsReadOnly(row, col)) {
      flag |= wxACC_STATE_SYSTEM_UNAVAILABLE;
   }
#endif

   *state = flag;

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus GridAx::GetValue(int childId, wxString *strValue)
{
   strValue->Clear();

#if defined(__WXMSW__)
   return wxACC_OK;
#endif

#if defined(__WXMAC__)
   return GetName(childId, strValue);
#endif
}

#if defined(__WXMAC__)
// Selects the object or child.
wxAccStatus GridAx::Select(int childId, wxAccSelectionFlags selectFlags)
{
   int row;
   int col;

   if (GetRowCol(childId, row, col)) {

      if (selectFlags & wxACC_SEL_TAKESELECTION) {
         mGrid->SetGridCursor(row, col);
      }

      mGrid->SelectBlock(row, col, row, col, selectFlags & wxACC_SEL_ADDSELECTION);
   }

   return wxACC_OK;
}
#endif

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus GridAx::GetFocus(int * WXUNUSED(childId), wxAccessible **child)
{
   *child = this;

   return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY
