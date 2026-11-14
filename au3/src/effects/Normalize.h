/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include "NormalizeBase.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>
#include <functional>

class wxCheckBox;
class wxStaticText;
class wxTextCtrl;
class ShuttleGui;

class EffectNormalize final : public NormalizeBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()

private:
    void OnUpdateUI(wxCommandEvent& evt);
    void UpdateUI();

    wxWeakRef<wxWindow> mUIParent{};

    wxCheckBox* mGainCheckBox;
    wxCheckBox* mDCCheckBox;
    wxTextCtrl* mLevelTextCtrl;
    wxStaticText* mLeveldB;
    wxStaticText* mWarning;
    wxCheckBox* mStereoIndCheckBox;
    bool mCreating;
};

#endif
