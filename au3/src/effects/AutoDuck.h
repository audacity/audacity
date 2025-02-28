/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.h

  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AUTODUCK__
#define __AUDACITY_EFFECT_AUTODUCK__

#include "AutoDuckBase.h"
#include "StatefulEffectUIServices.h"
#include <float.h> // for DBL_MAX
#include "wxPanelWrapper.h"

class wxBitmap;
class wxTextCtrl;
class ShuttleGui;
class WaveChannel;

#define AUTO_DUCK_PANEL_NUM_CONTROL_POINTS 5

class EffectAutoDuck final : public AutoDuckBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool DoTransferDataToWindow();
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()

private:
    void OnValueChanged(wxCommandEvent& evt);

    wxWeakRef<wxWindow> mUIParent{};

    wxTextCtrl* mDuckAmountDbBox;
    wxTextCtrl* mInnerFadeDownLenBox;
    wxTextCtrl* mInnerFadeUpLenBox;
    wxTextCtrl* mOuterFadeDownLenBox;
    wxTextCtrl* mOuterFadeUpLenBox;
    wxTextCtrl* mThresholdDbBox;
    wxTextCtrl* mMaximumPauseBox;

    class Panel;
    Panel* mPanel{};
};

class EffectAutoDuck::Panel final : public wxPanelWrapper
{
public:
    Panel(
        wxWindow* parent, wxWindowID winid, EffectAutoDuck* effect);
    virtual ~Panel();

private:
    enum EControlPoint
    {
        innerFadeDown = 0,
        outerFadeDown,
        innerFadeUp,
        outerFadeUp,
        duckAmount,
        none = 99,
    };

    bool AcceptsFocus() const override { return false; }
    // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
    bool AcceptsFocusFromKeyboard() const override { return false; }

    void OnPaint(wxPaintEvent& evt);
    void OnMouseCaptureChanged(wxMouseCaptureChangedEvent& evt);
    void OnMouseCaptureLost(wxMouseCaptureLostEvent& evt);
    void OnLeftDown(wxMouseEvent& evt);
    void OnLeftUp(wxMouseEvent& evt);
    void OnMotion(wxMouseEvent& evt);

    void ResetControlPoints();
    EControlPoint GetNearestControlPoint(const wxPoint& pt);

private:
    wxWindow* mParent;
    EffectAutoDuck* mEffect;
    std::unique_ptr<wxBitmap> mBackgroundBitmap;
    EControlPoint mCurrentControlPoint;
    wxPoint mControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
    wxPoint mMoveStartControlPoints[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
    wxPoint mMouseDownPoint;
    bool mControlPointMoveActivated;

    DECLARE_EVENT_TABLE()
};

#endif
