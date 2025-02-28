/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationPanel.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/

#ifndef __AUDACITY_EQUALIZATION_PANEL__
#define __AUDACITY_EQUALIZATION_PANEL__

#define PANELBORDER 1   // only increase from '1' for testing purposes - MJS

#include "SampleFormat.h"
#include "wxPanelWrapper.h"
class EnvelopeEditor;

class EqualizationCurvesList;
class RulerPanel;

class EqualizationPanel final : public wxPanelWrapper
{
public:
    EqualizationPanel(
        wxWindow* parent, wxWindowID winid, EqualizationCurvesList& curvesList, RulerPanel& freqRuler, RulerPanel& dbRuler);
    ~EqualizationPanel();

    // We don't need or want to accept focus.
    bool AcceptsFocus() const { return false; }
    // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
    bool AcceptsFocusFromKeyboard() const { return false; }

    void Recalc();

private:
    void OnIdle(wxIdleEvent& event);
    void OnMouseEvent(wxMouseEvent& event);
    void OnCaptureLost(wxMouseCaptureLostEvent& event);
    void OnPaint(wxPaintEvent& event);
    void OnSize(wxSizeEvent& event);

public:
//   int & mM;
//   float & mdBMax;
//   float & mdBMin;
//   Envelope & mEnvelope;

private:
    wxWindow* mParent;
    EqualizationCurvesList& mCurvesList;
    RulerPanel& mFreqRuler;
    RulerPanel& mdBRuler;
    std::unique_ptr<EnvelopeEditor> mLinEditor, mLogEditor;

    std::unique_ptr<wxBitmap> mBitmap;
    wxRect mEnvRect;
    int mWidth;
    int mHeight;
//   size_t mWindowSize;
//   float *mFilterFuncR;
//   float *mFilterFuncI;
    Floats mOutr;

//   double mLoFreq;
//   double mHiFreq;

    DECLARE_EVENT_TABLE()
};

#endif
