/**********************************************************************

Audacity: A Digital Audio Editor

EffectScienFilter.h

Norm C
Mitch Golden
Vaughan Johnson (Preview)

***********************************************************************/

#ifndef __AUDACITY_EFFECT_SCIENFILTER__
#define __AUDACITY_EFFECT_SCIENFILTER__

#include <wx/setup.h> // for wxUSE_* macros

#include "ScienFilterBase.h"
#include "StatefulEffectUIServices.h"
#include "wxPanelWrapper.h"

class wxBitmap;
class wxChoice;
class wxSlider;
class wxStaticText;
class wxTextCtrl;
class RulerPanel;
class ShuttleGui;

class EffectScienFilterPanel;

class EffectScienFilter final : public ScienFilterBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()
private:
    bool TransferGraphLimitsFromWindow();
    void EnableDisableRippleCtl(int FilterType);

    void OnSize(wxSizeEvent& evt);
    void OnSlider(wxCommandEvent& evt);

    void OnOrder(wxCommandEvent& evt);
    void OnCutoff(wxCommandEvent& evt);
    void OnRipple(wxCommandEvent& evt);
    void OnStopbandRipple(wxCommandEvent& evt);
    void OnFilterType(wxCommandEvent& evt);
    void OnFilterSubtype(wxCommandEvent& evt);

    void OnSliderDBMAX(wxCommandEvent& evt);
    void OnSliderDBMIN(wxCommandEvent& evt);

    wxWeakRef<wxWindow> mUIParent {};

    EffectScienFilterPanel* mPanel;
    wxSlider* mdBMinSlider;
    wxSlider* mdBMaxSlider;

    wxStaticText* mRippleCtlP;
    wxTextCtrl* mRippleCtl;
    wxStaticText* mRippleCtlU;

    wxTextCtrl* mCutoffCtl;

    wxStaticText* mStopbandRippleCtlP;
    wxTextCtrl* mStopbandRippleCtl;
    wxStaticText* mStopbandRippleCtlU;

    wxChoice* mFilterTypeCtl;
    wxChoice* mFilterSubTypeCtl;
    wxChoice* mFilterOrderCtl;

    RulerPanel* mdBRuler;
    RulerPanel* mfreqRuler;

    friend class EffectScienFilterPanel;
};

class EffectScienFilterPanel final : public wxPanelWrapper
{
public:
    EffectScienFilterPanel(
        wxWindow* parent, wxWindowID winid, EffectScienFilter* effect, double lo, double hi);
    virtual ~EffectScienFilterPanel();

    // We don't need or want to accept focus.
    bool AcceptsFocus() const;
    // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
    bool AcceptsFocusFromKeyboard() const;

    void SetFreqRange(double lo, double hi);
    void SetDbRange(double min, double max);

private:
    void OnPaint(wxPaintEvent& evt);
    void OnSize(wxSizeEvent& evt);

private:
    EffectScienFilter* mEffect;
    wxWindow* mParent;

    double mLoFreq;
    double mHiFreq;

    double mDbMin;
    double mDbMax;

    std::unique_ptr<wxBitmap> mBitmap;
    wxRect mEnvRect;
    int mWidth;
    int mHeight;

    friend class EffectScienFilter;

    DECLARE_EVENT_TABLE()
};

#endif
