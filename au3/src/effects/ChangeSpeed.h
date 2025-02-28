/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni

  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include "ChangeSpeedBase.h"
#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class wxSlider;
class wxChoice;
class wxTextCtrl;
class NumericTextCtrl;
class ShuttleGui;

class EffectChangeSpeed : public ChangeSpeedBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()
private:
    // handlers
    void OnText_PercentChange(wxCommandEvent& evt);
    void OnText_Multiplier(wxCommandEvent& evt);
    void OnSlider_PercentChange(wxCommandEvent& evt);
    void OnChoice_Vinyl(wxCommandEvent& evt);
    void OnTimeCtrl_ToLength(wxCommandEvent& evt);
    void OnTimeCtrlUpdate(wxCommandEvent& evt);

    // helper functions
    void
    Update_Text_PercentChange();  // Update control per current m_PercentChange.
    void Update_Text_Multiplier(); // Update control per current m_PercentChange.
    void
    Update_Slider_PercentChange(); // Update control per current m_PercentChange.
    void Update_Vinyl(); // Update Vinyl controls for NEW percent change.
    void Update_TimeCtrl_ToLength(); // Update target length controls for NEW
                                     // percent change.
    void UpdateUI();                // Enable / disable OK / preview.

    wxWeakRef<wxWindow> mUIParent {};

    // controls
    wxTextCtrl* mpTextCtrl_PercentChange;
    wxTextCtrl* mpTextCtrl_Multiplier;
    wxSlider* mpSlider_PercentChange;
    wxChoice* mpChoice_FromVinyl;
    wxChoice* mpChoice_ToVinyl;
    NumericTextCtrl* mpFromLengthCtrl;
    NumericTextCtrl* mpToLengthCtrl;
};

#endif // __AUDACITY_EFFECT_CHANGESPEED__
