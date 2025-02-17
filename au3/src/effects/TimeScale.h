/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.h

  Clayton Otey

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TIMESCALE__
#define __AUDACITY_EFFECT_TIMESCALE__

#if USE_SBSMS

#include "StatefulEffectUIServices.h"
#include "TimeScaleBase.h"
#include <wx/weakref.h>

class wxSlider;
class wxTextCtrl;
class ShuttleGui;

class EffectTimeScale : public TimeScaleBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()
private:

    void OnText_RatePercentChangeStart(wxCommandEvent& evt);
    void OnText_RatePercentChangeEnd(wxCommandEvent& evt);
    void OnText_PitchPercentChangeStart(wxCommandEvent& evt);
    void OnText_PitchPercentChangeEnd(wxCommandEvent& evt);
    void OnText_PitchHalfStepsStart(wxCommandEvent& evt);
    void OnText_PitchHalfStepsEnd(wxCommandEvent& evt);
    void OnSlider_RatePercentChangeStart(wxCommandEvent& evt);
    void OnSlider_RatePercentChangeEnd(wxCommandEvent& evt);
    void OnCheckBox_PreAnalyze(wxCommandEvent& evt);

    void Update_Text_RatePercentChangeStart();
    void Update_Text_RatePercentChangeEnd();
    void Update_Text_PitchPercentChangeStart();
    void Update_Text_PitchPercentChangeEnd();
    void Update_Text_PitchHalfStepsStart();
    void Update_Text_PitchHalfStepsEnd();
    void Update_Slider_RatePercentChangeStart();
    void Update_Slider_RatePercentChangeEnd();

    wxWeakRef<wxWindow> mUIParent{};

    wxTextCtrl* m_pTextCtrl_RatePercentChangeStart;
    wxTextCtrl* m_pTextCtrl_RatePercentChangeEnd;
    wxSlider* m_pSlider_RatePercentChangeStart;
    wxSlider* m_pSlider_RatePercentChangeEnd;
    wxTextCtrl* m_pTextCtrl_PitchHalfStepsStart;
    wxTextCtrl* m_pTextCtrl_PitchHalfStepsEnd;
    wxTextCtrl* m_pTextCtrl_PitchPercentChangeStart;
    wxTextCtrl* m_pTextCtrl_PitchPercentChangeEnd;
};

#endif // __AUDACITY_EFFECT_TIMESCALE

#endif // USE_SBSMS
