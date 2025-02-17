/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2 or later.  See License.txt.

  ChangePitch.h
  Vaughan Johnson, Dominic Mazzoni, Steve Daulton

************************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGEPITCH__
#define __AUDACITY_EFFECT_CHANGEPITCH__

#include "ChangePitchBase.h"
#include "StatefulEffectUIServices.h"

#include <wx/weakref.h>

class wxSlider;
class wxChoice;
class wxCheckBox;
class wxTextCtrl;
class wxSpinCtrl;
class ShuttleGui;

class EffectChangePitch : public ChangePitchBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()
private:
    // handlers
    void OnChoice_FromPitch(wxCommandEvent& evt);
    void OnSpin_FromOctave(wxCommandEvent& evt);
    void OnChoice_ToPitch(wxCommandEvent& evt);
    void OnSpin_ToOctave(wxCommandEvent& evt);

    void OnText_SemitonesChange(wxCommandEvent& evt);

    void OnText_FromFrequency(wxCommandEvent& evt);
    void OnText_ToFrequency(wxCommandEvent& evt);

    void OnText_PercentChange(wxCommandEvent& evt);
    void OnSlider_PercentChange(wxCommandEvent& evt);

    // helper fns for controls
    void Update_Choice_FromPitch();
    void Update_Spin_FromOctave();
    void Update_Choice_ToPitch();
    void Update_Spin_ToOctave();

    void Update_Text_SemitonesChange();

    void Update_Text_FromFrequency();
    void Update_Text_ToFrequency();

    void Update_Text_PercentChange(); // Update control per current m_dPercentChange.
    void Update_Slider_PercentChange(); // Update control per current m_dPercentChange.

    wxWeakRef<wxWindow> mUIParent{};

    // controls
    wxChoice* m_pChoice_FromPitch = nullptr;
    wxSpinCtrl* m_pSpin_FromOctave = nullptr;
    wxChoice* m_pChoice_ToPitch = nullptr;
    wxSpinCtrl* m_pSpin_ToOctave = nullptr;
    wxTextCtrl* m_pTextCtrl_SemitonesChange = nullptr;

    wxTextCtrl* m_pTextCtrl_FromFrequency = nullptr;
    wxTextCtrl* m_pTextCtrl_ToFrequency = nullptr;
    wxTextCtrl* m_pTextCtrl_PercentChange = nullptr;
    wxSlider* m_pSlider_PercentChange = nullptr;

#if USE_SBSMS
    wxCheckBox* mUseSBSMSCheckBox = nullptr;
#endif
};

#   endif // __AUDACITY_EFFECT_CHANGEPITCH__

#endif // USE_SOUNDTOUCH
