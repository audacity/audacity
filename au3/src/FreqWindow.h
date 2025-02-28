/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqWindow.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_FREQ_WINDOW__
#define __AUDACITY_FREQ_WINDOW__

#include "PlotSpectrumBase.h"
#include <vector>
#include <wx/font.h> // member variable
#include <wx/statusbr.h> // to inherit
#include "wxPanelWrapper.h" // to inherit

class wxMemoryDC;
class wxScrollBar;
class wxSlider;
class wxTextCtrl;
class wxButton;
class wxCheckBox;
class wxChoice;

class AudacityProject;
class FrequencyPlotDialog;
class FreqGauge;
class RulerPanel;

DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_FREQWINDOW_RECALC, -1);

class FreqPlot final : public wxWindow
{
public:
    FreqPlot(wxWindow* parent, wxWindowID winid);

    // We don't need or want to accept focus.
    bool AcceptsFocus() const;

private:
    void OnPaint(wxPaintEvent& event);
    void OnErase(wxEraseEvent& event);
    void OnMouseEvent(wxMouseEvent& event);

private:
    FrequencyPlotDialog* freqWindow;

    DECLARE_EVENT_TABLE()
};

class FrequencyPlotDialog final : public PlotSpectrumBase, public wxDialogWrapper, public PrefsListener
{
public:
    FrequencyPlotDialog(wxWindow* parent, wxWindowID id, AudacityProject& project, const TranslatableString& title, const wxPoint& pos);
    virtual ~FrequencyPlotDialog();

    bool Show(bool show = true) override;

private:
    void Populate();

    void PlotMouseEvent(wxMouseEvent& event);
    void PlotPaint(wxPaintEvent& event);

    void OnCloseWindow(wxCloseEvent& event);
    void OnCloseButton(wxCommandEvent& event);
    void OnGetURL(wxCommandEvent& event);
    void OnSize(wxSizeEvent& event);
    void OnPanScroller(wxScrollEvent& event);
    void OnZoomSlider(wxCommandEvent& event);
    void OnAlgChoice(wxCommandEvent& event);
    void OnSizeChoice(wxCommandEvent& event);
    void OnFuncChoice(wxCommandEvent& event);
    void OnAxisChoice(wxCommandEvent& event);
    void OnExport(wxCommandEvent& event);
    void OnReplot(wxCommandEvent& event);
    void OnGridOnOff(wxCommandEvent& event);
    void OnRecalc(wxCommandEvent& event);

    void SendRecalcEvent();
    void Recalc();
    void DrawPlot();
    void DrawBackground(wxMemoryDC& dc);

    // PrefsListener implementation
    void UpdatePrefs() override;

private:
#ifdef __WXMSW__
    static const int fontSize = 8;
#else
    static const int fontSize = 10;
#endif

    RulerPanel* vRuler;
    RulerPanel* hRuler;
    FreqPlot* mFreqPlot;
    FreqGauge* mProgress;

    wxRect mPlotRect;

    wxFont mFreqFont;

    std::unique_ptr<wxCursor> mArrowCursor;
    std::unique_ptr<wxCursor> mCrossCursor;

    wxButton* mCloseButton;
    wxButton* mExportButton;
    wxButton* mReplotButton;
    wxCheckBox* mGridOnOff;
    wxChoice* mAlgChoice;
    wxChoice* mSizeChoice;
    wxChoice* mFuncChoice;
    wxChoice* mAxisChoice;
    wxScrollBar* mPanScroller;
    wxSlider* mZoomSlider;
    wxTextCtrl* mCursorText;
    wxTextCtrl* mPeakText;

    bool mLogAxis;
    float mYMin;
    float mYMax;
    float mYStep;

    std::unique_ptr<wxBitmap> mBitmap;

    int mMouseX;
    int mMouseY;

    DECLARE_EVENT_TABLE()

    friend class FreqPlot;
};

#endif
