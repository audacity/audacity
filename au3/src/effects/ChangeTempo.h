/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.h

  Vaughan Johnson, Dominic Mazzoni

  Change Tempo effect provides speeding up or
  slowing down tempo without changing pitch.

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGETEMPO__
#define __AUDACITY_EFFECT_CHANGETEMPO__

#include "ChangeTempoBase.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class wxSlider;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

class EffectChangeTempo : public ChangeTempoBase, public StatefulEffectUIServices
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
    void OnSlider_PercentChange(wxCommandEvent& evt);
    void OnText_FromBPM(wxCommandEvent& evt);
    void OnText_ToBPM(wxCommandEvent& evt);
    void OnText_ToLength(wxCommandEvent& evt);

    // helper fns
    void Update_Text_PercentChange(); // Update control per current m_PercentChange.
    void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
    void Update_Text_ToBPM(); // Use m_FromBPM & m_PercentChange to set NEW m_ToBPM & control.
    void Update_Text_ToLength(); // Use m_FromLength & m_PercentChange to set NEW m_ToLength & control.

    wxWeakRef<wxWindow> mUIParent{};

    // controls
    wxTextCtrl* m_pTextCtrl_PercentChange;
    wxSlider* m_pSlider_PercentChange;
    wxTextCtrl* m_pTextCtrl_FromBPM;
    wxTextCtrl* m_pTextCtrl_ToBPM;
    wxTextCtrl* m_pTextCtrl_FromLength;
    wxTextCtrl* m_pTextCtrl_ToLength;

#if USE_SBSMS
    wxCheckBox* mUseSBSMSCheckBox;
#endif
};

#endif // __AUDACITY_EFFECT_CHANGETEMPO__

#endif // USE_SOUNDTOUCH
