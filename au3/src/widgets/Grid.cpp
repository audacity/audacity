/**********************************************************************

  Audacity: A Digital Audio Editor

  Grid.cpp

  Leland Lucius

*******************************************************************//**

\class Grid
\brief Supplies an accessible grid based on wxGrid.

*//*******************************************************************/
#include "Grid.h"

#include <wx/setup.h> // for wxUSE_* macros

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/clipbrd.h>
#include <wx/dc.h>
#include <wx/grid.h>
#include <wx/settings.h>
#include <wx/toplevel.h>

#include "IteratorX.h"
#include "NumericConverterFormats.h"

#include "SelectedRegion.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"

/**********************************************************************//**
\class GridAx
\brief wxAccessible object providing grid information for Grid.
**************************************************************************/
class GridAx final : public WindowAccessible
{
public:

    GridAx(const FormatterContext& project, Grid* grid);

    void SetCurrentCell(int row, int col);
    void TableUpdated();
    bool GetRowCol(int childId, int& row, int& col);

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
    wxAccStatus GetDefaultAction(int childId, wxString* actionName) override;

    // Returns the description for this object or a child.
    wxAccStatus GetDescription(int childId, wxString* description) override;

    // Gets the window with the keyboard focus.
    // If childId is 0 and child is NULL, no object in
    // this subhierarchy has the focus.
    // If this object has the focus, child should be 'this'.
    wxAccStatus GetFocus(int* childId, wxAccessible** child) override;

    // Returns help text for this object or a child, similar to tooltip text.
    wxAccStatus GetHelpText(int childId, wxString* helpText) override;

    // Returns the keyboard shortcut for this object or child.
    // Return e.g. ALT+K
    wxAccStatus GetKeyboardShortcut(int childId, wxString* shortcut) override;

    // Returns the rectangle for this object (id = 0) or a child element (id > 0).
    // rect is in screen coordinates.
    wxAccStatus GetLocation(wxRect& rect, int elementId) override;

    // Gets the name of the specified object.
    wxAccStatus GetName(int childId, wxString* name) override;

    // Gets the parent, or NULL.
    wxAccStatus GetParent(wxAccessible** parent) override;

    // Returns a role constant.
    wxAccStatus GetRole(int childId, wxAccRole* role) override;

    // Gets a variant representing the selected children
    // of this object.
    // Acceptable values:
    // - a null variant (IsNull() returns TRUE)
    // - a list variant (GetType() == wxT("list"))
    // - an integer representing the selected child element,
    //   or 0 if this object is selected (GetType() == wxT("long"))
    // - a "void*" pointer to a wxAccessible child object
    wxAccStatus GetSelections(wxVariant* selections) override;

    // Returns a state constant.
    wxAccStatus GetState(int childId, long* state) override;

    // Returns a localized string representing the value for the object
    // or child.
    wxAccStatus GetValue(int childId, wxString* strValue) override;

#if defined(__WXMAC__)
    // Selects the object or child.
    wxAccStatus Select(int childId, wxAccSelectionFlags selectFlags) override;
#endif
    FormatterContext mContext;

    Grid* mGrid;
    int mLastId;
};
#endif

NumericEditor::NumericEditor(
    const FormatterContext& context, NumericConverterType type,
    const NumericFormatID& format)
    : mContext{context}
{
    mType = std::move(type);
    mFormat = format;
    mOld = 0.0;
}

NumericEditor::~NumericEditor()
{
}

void NumericEditor::Create(wxWindow* parent, wxWindowID id, wxEvtHandler* handler)
{
    wxASSERT(parent); // to justify safenew
    auto control = safenew NumericTextCtrl(mContext,
                                           parent, wxID_ANY,
                                           mType,
                                           mFormat,
                                           mOld,
                                           NumericTextCtrl::Options{}
                                           .AutoPos(true)
                                           .InvalidValue(mType == NumericConverterType_FREQUENCY(),
                                                         SelectedRegion::UndefinedFrequency)
                                           );
    m_control = control;

    wxGridCellEditor::Create(parent, id, handler);
}

void NumericEditor::SetSize(const wxRect& rect)
{
    wxSize size = m_control->GetSize();

    // Always center...looks bad otherwise
    int x = rect.x + ((rect.width / 2) - (size.x / 2)) + 1;
    int y = rect.y + ((rect.height / 2) - (size.y / 2)) + 1;

    m_control->Move(x, y);
}

void NumericEditor::BeginEdit(int row, int col, wxGrid* grid)
{
    wxGridTableBase* table = grid->GetTable();

    mOldString = table->GetValue(row, col);
    mOldString.ToDouble(&mOld);

    auto control = GetNumericTextControl();
    control->SetValue(mOld);
    control->EnableMenu();

    control->SetFocus();
}

bool NumericEditor::EndEdit(int WXUNUSED(row), int WXUNUSED(col), const wxGrid* WXUNUSED(grid), const wxString& WXUNUSED(
                                oldval), wxString* newval)
{
    double newtime = GetNumericTextControl()->GetValue();
    bool changed = newtime != mOld;

    if (changed) {
        mValueAsString = wxString::Format(wxT("%g"), newtime);
        *newval = mValueAsString;
    }

    return changed;
}

void NumericEditor::ApplyEdit(int row, int col, wxGrid* grid)
{
    grid->GetTable()->SetValue(row, col, mValueAsString);
}

void NumericEditor::Reset()
{
    GetNumericTextControl()->SetValue(mOld);
}

bool NumericEditor::IsAcceptedKey(wxKeyEvent& event)
{
    if (wxGridCellEditor::IsAcceptedKey(event)) {
        if (event.GetKeyCode() == WXK_RETURN) {
            return true;
        }
    }

    return false;
}

// Clone is required by wxwidgets; implemented via copy constructor
wxGridCellEditor* NumericEditor::Clone() const
{
    return safenew NumericEditor{ mContext, mType, mFormat };
}

wxString NumericEditor::GetValue() const
{
    return wxString::Format(wxT("%g"), GetNumericTextControl()->GetValue());
}

NumericFormatID NumericEditor::GetFormat() const
{
    return mFormat;
}

void NumericEditor::SetFormat(const NumericFormatID& format)
{
    mFormat = format;
}

NumericRenderer::~NumericRenderer()
{
}

void NumericRenderer::Draw(wxGrid& grid,
                           wxGridCellAttr& attr,
                           wxDC& dc,
                           const wxRect& rect,
                           int row,
                           int col,
                           bool isSelected)
{
    wxGridCellRenderer::Draw(grid, attr, dc, rect, row, col, isSelected);

    wxGridTableBase* table = grid.GetTable();
    NumericEditor* ne
        =static_cast<NumericEditor*>(grid.GetCellEditor(row, col));
    wxString tstr;

    if (ne) {
        double value;

        table->GetValue(row, col).ToDouble(&value);

        NumericTextCtrl tt(mContext, &grid, wxID_ANY,
                           mType,
                           ne->GetFormat(),
                           value,
                           NumericTextCtrl::Options{}.AutoPos(true),
                           wxPoint(10000, 10000)); // create offscreen
        tstr = tt.GetString();

        ne->DecRef();
    }

    dc.SetBackgroundMode(wxTRANSPARENT);

    if (grid.IsEnabled()) {
        if (isSelected) {
            dc.SetTextBackground(grid.GetSelectionBackground());
            dc.SetTextForeground(grid.GetSelectionForeground());
        } else {
            dc.SetTextBackground(attr.GetBackgroundColour());
            dc.SetTextForeground(attr.GetTextColour());
        }
    } else {
        dc.SetTextBackground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
        dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_GRAYTEXT));
    }

    dc.SetFont(attr.GetFont());

    int hAlign, vAlign;

    attr.GetAlignment(&hAlign, &vAlign);

    grid.DrawTextRectangle(dc, tstr, rect, hAlign, vAlign);
}

wxSize NumericRenderer::GetBestSize(wxGrid& grid,
                                    wxGridCellAttr& WXUNUSED(attr),
                                    wxDC& WXUNUSED(dc),
                                    int row,
                                    int col)
{
    wxGridTableBase* table = grid.GetTable();
    NumericEditor* ne
        =static_cast<NumericEditor*>(grid.GetCellEditor(row, col));
    wxSize sz;

    if (ne) {
        double value;
        table->GetValue(row, col).ToDouble(&value);
        NumericTextCtrl tt(mContext, &grid, wxID_ANY,
                           mType,
                           ne->GetFormat(),
                           value,
                           NumericTextCtrl::Options{}.AutoPos(true),
                           wxPoint(10000, 10000)); // create offscreen
        sz = tt.GetSize();

        ne->DecRef();
    }

    return sz;
}

// Clone is required by wxwidgets; implemented via copy constructor
wxGridCellRenderer* NumericRenderer::Clone() const
{
    return safenew NumericRenderer{ mContext, mType };
}

ChoiceEditor::ChoiceEditor(size_t count, const wxString choices[])
{
    if (count) {
        mChoices.reserve(count);
        for (size_t n = 0; n < count; n++) {
            mChoices.push_back(choices[n]);
        }
    }
}

ChoiceEditor::ChoiceEditor(const wxArrayString& choices)
{
    mChoices = choices;
}

ChoiceEditor::~ChoiceEditor()
{
    if (m_control) {
        mHandler.DisconnectEvent(m_control);
    }
}

// Clone is required by wxwidgets; implemented via copy constructor
wxGridCellEditor* ChoiceEditor::Clone() const
{
    return safenew ChoiceEditor(mChoices);
}

void ChoiceEditor::Create(wxWindow* parent, wxWindowID id, wxEvtHandler* evtHandler)
{
    m_control = safenew wxChoice(parent,
                                 id,
                                 wxDefaultPosition,
                                 wxDefaultSize,
                                 mChoices);

    wxGridCellEditor::Create(parent, id, evtHandler);
    mHandler.ConnectEvent(m_control);
}

void ChoiceEditor::SetSize(const wxRect& rect)
{
    wxSize size = m_control->GetSize();

    // Always center...looks bad otherwise
    int x = rect.x + ((rect.width / 2) - (size.x / 2)) + 1;
    int y = rect.y + ((rect.height / 2) - (size.y / 2)) + 1;

    m_control->Move(x, y);
}

void ChoiceEditor::BeginEdit(int row, int col, wxGrid* grid)
{
    if (!m_control) {
        return;
    }

    mOld = grid->GetTable()->GetValue(row, col);

    Choice()->Clear();
    Choice()->Append(mChoices);
    Choice()->SetSelection(make_iterator_range(mChoices).index(mOld));
    Choice()->SetFocus();
}

bool ChoiceEditor::EndEdit(int row, int col, wxGrid* grid)
{
    wxString newvalue;
    bool changed = EndEdit(row, col, grid, mOld, &newvalue);
    if (changed) {
        ApplyEdit(row, col, grid);
    }
    return changed;
}

bool ChoiceEditor::EndEdit(int WXUNUSED(row), int WXUNUSED(col),
                           const wxGrid* WXUNUSED(grid),
                           const wxString& WXUNUSED(oldval), wxString* newval)
{
    int sel = Choice()->GetSelection();

    // This can happen if the wxChoice control is displayed and the list of choices get changed
    if ((sel < 0) || (sel >= (int)(mChoices.size()))) {
        return false;
    }

    wxString val = mChoices[sel];
    bool changed = val != mOld;

    if (changed) {
        mValueAsString = val;
        *newval = val;
    }

    return changed;
}

void ChoiceEditor::ApplyEdit(int row, int col, wxGrid* grid)
{
    grid->GetTable()->SetValue(row, col, mValueAsString);
}

void ChoiceEditor::Reset()
{
    Choice()->SetSelection(make_iterator_range(mChoices).index(mOld));
}

void ChoiceEditor::SetChoices(const wxArrayString& choices)
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
EVT_GRID_EDITOR_SHOWN(Grid::OnEditorShown)
END_EVENT_TABLE()

Grid::Grid(
    const FormatterContext& context,
    wxWindow* parent,
    wxWindowID id,
    const wxPoint& pos,
    const wxSize& size,
    long style,
    const wxString& name)
    : wxGrid(parent, id, pos, size, style | wxWANTS_CHARS, name), mContext(context)
{
#if wxUSE_ACCESSIBILITY
    GetGridWindow()->SetAccessible(mAx = safenew GridAx(mContext, this));
#endif

    // RegisterDataType takes ownership of renderer and editor

    RegisterDataType(GRID_VALUE_TIME,
                     safenew NumericRenderer { mContext, NumericConverterType_TIME() },
                     safenew NumericEditor
                     { mContext, NumericConverterType_TIME(),
                       NumericConverterFormats::SecondsFormat().Internal() });

    RegisterDataType(GRID_VALUE_FREQUENCY,
                     safenew NumericRenderer { mContext, NumericConverterType_FREQUENCY() },
                     safenew NumericEditor
                     { mContext, NumericConverterType_FREQUENCY(),
                       NumericConverterFormats::HertzFormat().Internal() });

    RegisterDataType(GRID_VALUE_CHOICE,
                     safenew wxGridCellStringRenderer,
                     safenew ChoiceEditor);

    // Bug #2803:
    // Ensure selection doesn't show up.
    SetSelectionForeground(GetDefaultCellTextColour());
    SetSelectionBackground(GetDefaultCellBackgroundColour());
}

Grid::~Grid()
{
#if wxUSE_ACCESSIBILITY
    int cnt = mChildren.size();
    while (cnt--) {
        // PRL: I found this loop destroying right-to-left.
        // Is the sequence of destruction important?
        mChildren.pop_back();
    }
#endif
}

void Grid::OnSetFocus(wxFocusEvent& event)
{
    event.Skip();

#if wxUSE_ACCESSIBILITY
    mAx->SetCurrentCell(GetGridCursorRow(), GetGridCursorCol());
#endif
}

void Grid::OnSelectCell(wxGridEvent& event)
{
    event.Skip();

    MakeCellVisible(event.GetRow(), event.GetCol());

#if wxUSE_ACCESSIBILITY
    mAx->SetCurrentCell(event.GetRow(), event.GetCol());
#endif
}

void Grid::OnEditorShown(wxGridEvent& event)
{
    event.Skip();

    // Bug #2803 (comment 7):
    // Select row whenever an editor is displayed
    SelectRow(GetGridCursorRow());
}

void Grid::OnKeyDown(wxKeyEvent& event)
{
    auto keyCode = event.GetKeyCode();
    int crow = GetGridCursorRow();
    int ccol = GetGridCursorCol();

    if (event.CmdDown() && crow != wxGridNoCellCoords.GetRow() && ccol != wxGridNoCellCoords.GetCol()) {
        wxClipboardLocker cb;

        switch (keyCode) {
        case 'C':  // Copy
        {
            wxTextDataObject* data = safenew wxTextDataObject(GetCellValue(crow, ccol));
            wxClipboard::Get()->SetData(data);
            return;
        }
        break;

        case 'X':  // Cut
        {
            wxTextDataObject* data = safenew wxTextDataObject(GetCellValue(crow, ccol));
            wxClipboard::Get()->SetData(data);
            SetCellValue(crow, ccol, "");
            return;
        }
        break;

        case 'V':  // Paste
        {
            if (wxClipboard::Get()->IsSupported(wxDF_UNICODETEXT)) {
                wxTextDataObject data;
                if (wxClipboard::Get()->GetData(data)) {
                    SetCellValue(crow, ccol, data.GetText());
                    return;
                }
            }
        }
        break;
        }
    }

    switch (keyCode) {
    case WXK_LEFT:
    case WXK_RIGHT:
    {
        int rows = GetNumberRows();
        int cols = GetNumberCols();

        const bool has_cells = rows > 0 && cols > 0;

        if (has_cells) {
            int crow = GetGridCursorRow();
            int ccol = GetGridCursorCol();

            const bool has_no_selection = crow == wxGridNoCellCoords.GetRow() || ccol == wxGridNoCellCoords.GetCol();

            if (has_no_selection) {
                SetGridCursor(0, 0);
            } else if (event.GetKeyCode() == WXK_LEFT) {
                if (crow == 0 && ccol == 0) {
                    // do nothing
                } else if (ccol == 0) {
                    SetGridCursor(crow - 1, cols - 1);
                } else {
                    SetGridCursor(crow, ccol - 1);
                }
            } else {
                if (crow == rows - 1 && ccol == cols - 1) {
                    // do nothing
                } else if (ccol == cols - 1) {
                    SetGridCursor(crow + 1, 0);
                } else {
                    SetGridCursor(crow, ccol + 1);
                }
            }
        }

#if wxUSE_ACCESSIBILITY
        // Make sure the NEW cell is made available to the screen reader
        mAx->SetCurrentCell(GetGridCursorRow(), GetGridCursorCol());
#endif
    }
    break;

    case WXK_TAB:
    {
        if (event.ControlDown()) {
            int flags = wxNavigationKeyEvent::FromTab
                        | (event.ShiftDown()
                           ? wxNavigationKeyEvent::IsBackward
                           : wxNavigationKeyEvent::IsForward);
            Navigate(flags);
            return;
        }

        int rows = GetNumberRows();
        int cols = GetNumberCols();
        int crow = GetGridCursorRow();
        int ccol = GetGridCursorCol();

        const auto is_empty = rows <= 0 || cols <= 0;
        const auto has_no_selection = crow == wxGridNoCellCoords.GetRow() || ccol == wxGridNoCellCoords.GetCol();

        if (event.ShiftDown()) {
            if (is_empty) {
                Navigate(wxNavigationKeyEvent::FromTab | wxNavigationKeyEvent::IsBackward);
                return;
            }

            if (crow == 0 && ccol == 0) {
                Navigate(wxNavigationKeyEvent::FromTab | wxNavigationKeyEvent::IsBackward);
                return;
            }

            if (has_no_selection) {
                SetGridCursor(rows - 1, cols - 1);
            } else if (ccol == 0) {
                SetGridCursor(crow - 1, cols - 1);
            } else {
                SetGridCursor(crow, ccol - 1);
            }
        } else {
            if (is_empty) {
                Navigate(wxNavigationKeyEvent::FromTab | wxNavigationKeyEvent::IsForward);
                return;
            }

            if (crow == rows - 1 && ccol == cols - 1) {
                Navigate(wxNavigationKeyEvent::FromTab | wxNavigationKeyEvent::IsForward);
                return;
            }

            if (has_no_selection) {
                SetGridCursor(0, 0);
            } else if (ccol == cols - 1) {
                SetGridCursor(crow + 1, 0);
            } else {
                SetGridCursor(crow, ccol + 1);
            }
        }

        MakeCellVisible(GetGridCursorRow(), GetGridCursorCol());

#if wxUSE_ACCESSIBILITY
        // Make sure the NEW cell is made available to the screen reader
        mAx->SetCurrentCell(GetGridCursorRow(), GetGridCursorCol());
#endif
    }
    break;

    case WXK_RETURN:
    case WXK_NUMPAD_ENTER:
    {
        if (!IsCellEditControlShown()) {
            wxTopLevelWindow* tlw = wxDynamicCast(wxGetTopLevelParent(this), wxTopLevelWindow);
            wxWindow* def = tlw->GetDefaultItem();
            if (def && def->IsEnabled()) {
                wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                                      def->GetId());
                cevent.SetEventObject(def);
                GetParent()->GetEventHandler()->ProcessEvent(cevent);
            }
        } else {
            wxGrid::OnKeyDown(event);

            // This looks strange, but what it does is selects the cell when
            // enter is pressed after editing.  Without it, Jaws and Window-Eyes
            // do not speak the NEW cell contents (the one below the edited one).
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

GridAx::GridAx(const FormatterContext& context, Grid* grid)
    : WindowAccessible(grid->GetGridWindow())
    , mContext(context)
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

    if (mGrid == wxWindow::FindFocus()) {
        NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                    mGrid->GetGridWindow(),
                    wxOBJID_CLIENT,
                    id);
    }

    NotifyEvent(wxACC_EVENT_OBJECT_SELECTION,
                mGrid->GetGridWindow(),
                wxOBJID_CLIENT,
                id);

    mLastId = id;
}

bool GridAx::GetRowCol(int childId, int& row, int& col)
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
    } else {
        *child = NULL;
    }

    return wxACC_OK;
}

// Gets the number of children.
wxAccStatus GridAx::GetChildCount(int* childCount)
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
wxAccStatus GridAx::GetDefaultAction(int WXUNUSED(childId), wxString* actionName)
{
    actionName->clear();

    return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus GridAx::GetDescription(int WXUNUSED(childId), wxString* description)
{
    description->clear();

    return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus GridAx::GetHelpText(int WXUNUSED(childId), wxString* helpText)
{
    helpText->clear();

    return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus GridAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString* shortcut)
{
    shortcut->clear();

    return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus GridAx::GetLocation(wxRect& rect, int elementId)
{
    wxRect r;
    int row;
    int col;

    if (GetRowCol(elementId, row, col)) {
        rect = mGrid->CellToRect(row, col);
        rect.SetPosition(mGrid->CalcScrolledPosition(rect.GetPosition()));
        rect.SetPosition(mGrid->GetGridWindow()->ClientToScreen(rect.GetPosition()));
    } else {
        rect = mGrid->GetRect();
        rect.SetPosition(mGrid->GetParent()->ClientToScreen(rect.GetPosition()));
    }

    return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus GridAx::GetName(int childId, wxString* name)
{
    int row;
    int col;

    if (GetRowCol(childId, row, col)) {
        wxString n = mGrid->GetColLabelValue(col);
        wxString v = mGrid->GetCellValue(row, col);
        if (v.empty()) {
            v = _("Empty");
        }

        // Hack to provide a more intelligible response
        NumericEditor* dt
            =static_cast<NumericEditor*>(mGrid->GetDefaultEditorForType(GRID_VALUE_TIME));
        NumericEditor* df
            =static_cast<NumericEditor*>(mGrid->GetDefaultEditorForType(GRID_VALUE_FREQUENCY));
        NumericEditor* c
            =static_cast<NumericEditor*>(mGrid->GetCellEditor(row, col));

        if (c && dt && df && (c == dt || c == df)) {
            double value;
            v.ToDouble(&value);
            NumericConverter converter(mContext, c == dt ? NumericConverterType_TIME() : NumericConverterType_FREQUENCY(),
                                       c->GetFormat(),
                                       value);

            v = converter.GetString();
        }

        if (c) {
            c->DecRef();
        }
        if (dt) {
            dt->DecRef();
        }
        if (df) {
            df->DecRef();
        }

        *name = n + wxT(" ") + v;
    }

    return wxACC_OK;
}

wxAccStatus GridAx::GetParent(wxAccessible** WXUNUSED(parent))
{
    return wxACC_NOT_IMPLEMENTED;
}

// Returns a role constant.
wxAccStatus GridAx::GetRole(int childId, wxAccRole* role)
{
    if (childId == wxACC_SELF) {
#if defined(__WXMSW__)
        *role = wxROLE_SYSTEM_TABLE;
#endif

#if defined(__WXMAC__)
        *role = wxROLE_SYSTEM_GROUPING;
#endif
    } else {
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
wxAccStatus GridAx::GetSelections(wxVariant* WXUNUSED(selections))
{
    return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus GridAx::GetState(int childId, long* state)
{
    int flag = wxACC_STATE_SYSTEM_FOCUSABLE
               | wxACC_STATE_SYSTEM_SELECTABLE;
    int col;
    int row;

    if (!GetRowCol(childId, row, col)) {
        *state = 0;
        return wxACC_FAIL;
    }

#if defined(__WXMSW__)
    flag |= wxACC_STATE_SYSTEM_FOCUSED
            | wxACC_STATE_SYSTEM_SELECTED;

    if (mGrid->IsReadOnly(row, col)) {
        // It would be more logical to also include the state
        // wxACC_STATE_SYSTEM_FOCUSABLE, but this causes Window-Eyes to
        // no longer read the cell as disabled
        flag = wxACC_STATE_SYSTEM_UNAVAILABLE | wxACC_STATE_SYSTEM_FOCUSED;
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
#if defined(__WXMAC__)
wxAccStatus GridAx::GetValue(int childId, wxString* strValue)
#else
wxAccStatus GridAx::GetValue(int WXUNUSED(childId), wxString* strValue)
#endif
{
    strValue->clear();

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
wxAccStatus GridAx::GetFocus(int* childId, wxAccessible** child)
{
    if (mGrid == wxWindow::FindFocus()) {
        if (mGrid->GetNumberRows() * mGrid->GetNumberCols() == 0) {
            *child = this;
        } else {
            *childId = mGrid->GetGridCursorRow() * mGrid->GetNumberCols()
                       + mGrid->GetGridCursorCol() + 1;
        }
    }

    return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY
