/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.h

  Dominic Mazzoni

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AMPLIFY__
#define __AUDACITY_EFFECT_AMPLIFY__

#include "AmplifyBase.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class wxSlider;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

class EffectAmplify : public AmplifyBase, public StatefulEffectUIServices
{
public:
    std::shared_ptr<EffectInstance> MakeInstance() const override;

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()

    void OnAmpText(wxCommandEvent& evt);
    void OnPeakText(wxCommandEvent& evt);
    void OnAmpSlider(wxCommandEvent& evt);
    void OnClipCheckBox(wxCommandEvent& evt);

    void CheckClip();

private:
    wxWeakRef<wxWindow> mUIParent {};

    wxSlider* mAmpS;
    wxTextCtrl* mAmpT;
    wxTextCtrl* mNewPeakT;
    wxCheckBox* mClip;
};

#endif // __AUDACITY_EFFECT_AMPLIFY__
