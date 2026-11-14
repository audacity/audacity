/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPEAT__
#define __AUDACITY_EFFECT_REPEAT__

#include "RepeatBase.h"
#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class wxTextCtrl;
class ShuttleGui;

class wxStaticText;

class EffectRepeat final : public RepeatBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

private:
    void OnRepeatTextChange(wxCommandEvent& evt);
    void DisplayNewTime();

    wxWeakRef<wxWindow> mUIParent{};

    wxTextCtrl* mRepeatCount;
    wxStaticText* mCurrentTime;
    wxStaticText* mTotalTime;
    DECLARE_EVENT_TABLE()
};

#endif
