/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.h

  Craig DeForest

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CLICK_REMOVAL__
#define __AUDACITY_EFFECT_CLICK_REMOVAL__

#include "ClickRemovalBase.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class wxSlider;
class wxTextCtrl;
class ShuttleGui;

class EffectClickRemoval final : public ClickRemovalBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()

private:
    void OnWidthText(wxCommandEvent& evt);
    void OnThreshText(wxCommandEvent& evt);
    void OnWidthSlider(wxCommandEvent& evt);
    void OnThreshSlider(wxCommandEvent& evt);

private:
    wxWeakRef<wxWindow> mUIParent{};

    wxSlider* mWidthS;
    wxSlider* mThreshS;
    wxTextCtrl* mWidthT;
    wxTextCtrl* mThreshT;
};

#endif
