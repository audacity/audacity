/**********************************************************************

  Audacity: A Digital Audio Editor

  Loudness.h

  Max Maisel (based on Normalize effect)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_LOUDNESS__
#define __AUDACITY_EFFECT_LOUDNESS__

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/weakref.h>

#include "LoudnessBase.h"
#include "StatefulEffectUIServices.h"

class wxChoice;
class wxSimplebook;
class ShuttleGui;

class EffectLoudness final : public LoudnessBase, public StatefulEffectUIServices
{
public:
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;

    DECLARE_EVENT_TABLE()
private:
    void OnChoice(wxCommandEvent& evt);
    void OnUpdateUI(wxCommandEvent& evt);
    void UpdateUI();

    wxWeakRef<wxWindow> mUIParent {};

    wxSimplebook* mBook;
    wxChoice* mChoice;
    wxStaticText* mWarning;
    wxCheckBox* mStereoIndCheckBox;
    wxCheckBox* mDualMonoCheckBox;
};

#endif
