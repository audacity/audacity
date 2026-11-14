/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEditor.h

  Dominic Mazzoni

  Paul Licameli split from LadspaEffect.cpp

**********************************************************************/
#ifndef __AUDACITY_LADSPA_EDITOR__
#define __AUDACITY_LADSPA_EDITOR__

#include "../EffectEditor.h"
#include "LadspaInstance.h" // for LadspaEffectSettings

#include <wx/dialog.h>
#include <wx/weakref.h>

struct LadspaInstance;
class NumericTextCtrl;
class wxSlider;
class wxTextCtrl;
class wxStaticText;
class wxCheckBox;
class LadspaEffectMeter;

struct LadspaEditor : EffectEditor {
    //! Assume settings originated from MakeSettings() and copies thereof
    static inline LadspaEffectSettings& GetSettings(EffectSettings& settings)
    {
        auto pSettings = settings.cast<LadspaEffectSettings>();
        assert(pSettings);
        return *pSettings;
    }

    //! Assume settings originated from MakeSettings() and copies thereof
    static inline const LadspaEffectSettings&
    GetSettings(const EffectSettings& settings)
    {
        return GetSettings(const_cast<EffectSettings&>(settings));
    }

    LadspaEditor(const EffectUIServices& effect, const LadspaInstance& instance, unsigned numInputControls, unsigned numOutputControls,
                 EffectSettingsAccess& access, double sampleRate, EffectType type, const LadspaEffectOutputs* pOutputs);

    bool UpdateUI() override;
    bool ValidateUI() override;
    void Disconnect() override;

    void PopulateUI(ShuttleGui& S);

    void OnCheckBox(wxCommandEvent& evt);
    void OnSlider(wxCommandEvent& evt);
    void OnTextCtrl(wxCommandEvent& evt);
    void RefreshControls();

    void UpdateControl(int index, float value, float epsilon);
    void UpdateControls(const LadspaEffectSettings& src);

    const LadspaInstance& mInstance;
    const unsigned mNumInputControls;
    const unsigned mNumOutputControls;

    const double mSampleRate;
    const EffectType mType;
    LadspaEffectSettings mSettings;
    const LadspaEffectOutputs* const mpOutputs;

    NumericTextCtrl* mDuration{};
    wxWeakRef<wxDialog> mDialog;
    wxWindow* mParent{};
    ArrayOf<wxSlider*> mSliders;
    ArrayOf<wxTextCtrl*> mFields;
    ArrayOf<wxStaticText*> mLabels;
    ArrayOf<wxCheckBox*> mToggles;
    std::vector<LadspaEffectMeter*> mMeters;
};

#endif
