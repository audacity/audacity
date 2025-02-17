/**********************************************************************

  Audacity: A Digital Audio Editor

  Grid.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_GRID__
#define __AUDACITY_WIDGETS_GRID__

#include <vector>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/grid.h> // to inherit wxGridCellEditor
#include "NumericTextCtrl.h" // for NumericConverter::Type

class AudacityProject;

#if wxUSE_ACCESSIBILITY
class GridAx;
#endif

class wxArrayString;
class wxChoice;
class NumericTextCtrl;

/**********************************************************************//**

\class NumericEditor
\brief wxGridCellEditor for the NumericTextCtrl.

**************************************************************************/
#define GRID_VALUE_TIME wxT("Time")
#define GRID_VALUE_FREQUENCY wxT("Frequency")

class AUDACITY_DLL_API NumericEditor /* not final */ : public wxGridCellEditor
{
public:

    NumericEditor(const FormatterContext& context, NumericConverterType type, const NumericFormatID& format);

    ~NumericEditor();

    // Precondition: parent != NULL
    void Create(wxWindow* parent, wxWindowID id, wxEvtHandler* handler) override;

    bool IsAcceptedKey(wxKeyEvent& event) override;

    void SetSize(const wxRect& rect) override;

    void BeginEdit(int row, int col, wxGrid* grid) override;

    bool EndEdit(int row, int col, const wxGrid* grid, const wxString& oldval, wxString* newval) override;

    void ApplyEdit(int row, int col, wxGrid* grid) override;

    void Reset() override;

    NumericFormatID GetFormat() const;
    void SetFormat(const NumericFormatID& format);

    wxGridCellEditor* Clone() const override;
    wxString GetValue() const override;

    NumericTextCtrl* GetNumericTextControl() const
    { return static_cast<NumericTextCtrl*>(m_control); }

private:

    NumericFormatID mFormat;
    NumericConverterType mType;
    double mOld;
    wxString mOldString;
    wxString mValueAsString;

    FormatterContext mContext;
};

/**********************************************************************//**
\class NumericRenderer
\brief wxGridCellRenderer for the NumericTextCtrl.
**************************************************************************/
class NumericRenderer final : public wxGridCellRenderer
{
public:
    NumericRenderer(const FormatterContext& context, NumericConverterType type)
        : mType{std::move(type)}
        , mContext{context}
    {
    }

    ~NumericRenderer() override;

    void Draw(wxGrid& grid, wxGridCellAttr& attr, wxDC& dc, const wxRect& rect, int row, int col, bool isSelected) override;

    wxSize GetBestSize(wxGrid& grid, wxGridCellAttr& attr, wxDC& dc, int row, int col) override;

    wxGridCellRenderer* Clone() const override;

private:
    NumericConverterType mType;
    FormatterContext mContext;
};

/**********************************************************************//**
\class ChoiceEditor
\brief Modified version of wxGridChoiceEditor using wxChoice instead of
wxComboBox.
**************************************************************************/
#define GRID_VALUE_CHOICE wxT("Choice")

class AUDACITY_DLL_API ChoiceEditor final : public wxGridCellEditor, wxEvtHandler
{
public:

    ChoiceEditor(size_t count = 0, const wxString choices[] = NULL);
    ChoiceEditor(const wxArrayString& choices);

    ~ChoiceEditor();

    void Create(wxWindow* parent, wxWindowID id, wxEvtHandler* evtHandler) override;

    void SetSize(const wxRect& rect) override;
    void BeginEdit(int row, int col, wxGrid* grid) override;
    bool EndEdit(int row, int col, wxGrid* grid);
    bool EndEdit(int row, int col, const wxGrid* grid, const wxString& oldval, wxString* newval) override;
    void ApplyEdit(int row, int col, wxGrid* grid) override;
    void Reset() override;

    wxGridCellEditor* Clone() const override;

    void SetChoices(const wxArrayString& choices);
    wxString GetValue() const override;

protected:

    wxChoice* Choice() const { return (wxChoice*)m_control; }

private:

    // A whole separate class just to get rid of Visual C++ warning C4407
    class FocusHandler : wxEvtHandler
    {
    public:
        void ConnectEvent(wxWindow* w)
        {
            // Need to use a named function pointer, not a lambda, so that we
            // can unbind the same later
            w->GetEventHandler()->Bind(wxEVT_KILL_FOCUS, OnKillFocus);
        }

        void DisconnectEvent(wxWindow* w)
        {
            w->GetEventHandler()->Unbind(wxEVT_KILL_FOCUS, OnKillFocus);
        }

        static void OnKillFocus(wxFocusEvent& WXUNUSED(event))
        {
            return;
        }
    } mHandler;

    wxArrayString mChoices;
    wxString mOld;
    wxString mValueAsString;
};

/**********************************************************************//**
\class Grid
\brief wxGrid with support for accessibility.
**************************************************************************/

class AUDACITY_DLL_API Grid final : public wxGrid
{
public:

    Grid(
        const FormatterContext& context, wxWindow* parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize, long style = wxWANTS_CHARS | wxBORDER, const wxString& name = wxPanelNameStr);

    ~Grid();

#if wxUSE_ACCESSIBILITY
    void ClearGrid();
    bool InsertRows(int pos = 0, int numRows = 1, bool updateLabels = true);
    bool AppendRows(int numRows = 1, bool updateLabels = true);
    bool DeleteRows(int pos = 0, int numRows = 1, bool updateLabels = true);
    bool InsertCols(int pos = 0, int numCols = 1, bool updateLabels = true);
    bool AppendCols(int numCols = 1, bool updateLabels = true);
    bool DeleteCols(int pos = 0, int numCols = 1, bool updateLabels = true);

    GridAx* GetNextAx(GridAx* parent, wxAccRole role, int row, int col);
#endif

protected:

    void OnSetFocus(wxFocusEvent& event);
    void OnSelectCell(wxGridEvent& event);
    void OnEditorShown(wxGridEvent& event);
    void OnKeyDown(wxKeyEvent& event);

private:

    FormatterContext mContext;

#if wxUSE_ACCESSIBILITY
    GridAx* mAx;
    std::vector<std::unique_ptr<GridAx> > mChildren;
    int mObjNdx;
#endif

public:

    DECLARE_EVENT_TABLE()
};

#endif
