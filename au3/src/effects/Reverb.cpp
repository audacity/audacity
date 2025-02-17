/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   Reverb.cpp
   Rob Sykes, Vaughan Johnson

******************************************************************//**

\class EffectReverb
\brief A reverberation effect

*//*******************************************************************/
#include "Reverb.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <wx/arrstr.h>
#include <wx/checkbox.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "../widgets/valnum.h"

namespace {
BuiltinEffectsModule::Registration< EffectReverb > reg;
}

struct EffectReverb::Editor : EffectEditor
{
    Editor(const EffectUIServices& services,
           EffectSettingsAccess& access, const ReverbSettings& settings)
        : EffectEditor{services, access}
        , mSettings{settings}
    {}
    virtual ~Editor() = default;

    bool ValidateUI() override;
    bool UpdateUI() override;

    void PopulateOrExchange(ShuttleGui& S);

    ReverbSettings mSettings;

    bool mProcessingEvent = false;

#define SpinSlider(n) \
    wxSpinCtrl* m##n##T; \
    wxSlider* m##n##S;

    SpinSlider(RoomSize)
    SpinSlider(PreDelay)
    SpinSlider(Reverberance)
    SpinSlider(HfDamping)
    SpinSlider(ToneLow)
    SpinSlider(ToneHigh)
    SpinSlider(WetGain)
    SpinSlider(DryGain)
    SpinSlider(StereoWidth)

#undef SpinSlider

    wxCheckBox * mWetOnlyC;

#define SpinSliderHandlers(n) \
    void On##n##Slider(wxCommandEvent & evt); \
    void On##n##Text(wxCommandEvent & evt);

    SpinSliderHandlers(RoomSize)
    SpinSliderHandlers(PreDelay)
    SpinSliderHandlers(Reverberance)
    SpinSliderHandlers(HfDamping)
    SpinSliderHandlers(ToneLow)
    SpinSliderHandlers(ToneHigh)
    SpinSliderHandlers(WetGain)
    SpinSliderHandlers(DryGain)
    SpinSliderHandlers(StereoWidth)

#undef SpinSliderHandlers

    void OnCheckbox(wxCommandEvent& evt);
};

bool EffectReverb::Editor::ValidateUI()
{
    auto& rs = mSettings;

    rs.mRoomSize     = mRoomSizeS->GetValue();
    rs.mPreDelay     = mPreDelayS->GetValue();
    rs.mReverberance = mReverberanceS->GetValue();
    rs.mHfDamping    = mHfDampingS->GetValue();
    rs.mToneLow      = mToneLowS->GetValue();
    rs.mToneHigh     = mToneHighS->GetValue();
    rs.mWetGain      = mWetGainS->GetValue();
    rs.mDryGain      = mDryGainS->GetValue();
    rs.mStereoWidth  = mStereoWidthS->GetValue();
    rs.mWetOnly      = mWetOnlyC->GetValue();

    mAccess.ModifySettings
    (
        [this](EffectSettings& settings)
    {
        // pass back the modified settings to the MessageBuffer

        ReverbBase::GetSettings(settings) = mSettings;
        return nullptr;
    }
    );

    return true;
}

std::shared_ptr<EffectInstance>
EffectReverb::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

// Effect implementation
std::unique_ptr<EffectEditor> EffectReverb::MakeEditor(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*) const
{
    auto& settings = access.Get();
    auto& myEffSettings = GetSettings(settings);

    auto result = std::make_unique<Editor>(*this, access, myEffSettings);
    result->PopulateOrExchange(S);
    return result;
}

void EffectReverb::Editor::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(3, wxEXPAND);
    {
        S.SetStretchyCol(2);

#define SpinSlider(n, p) \
    m##n##T = S.AddSpinCtrl(p, n.def, n.max, n.min); \
    BindTo(*m##n##T, wxEVT_SPINCTRL, &Editor::On##n##Text); \
      \
    m##n##S = S.Style(wxSL_HORIZONTAL).AddSlider({}, n.def, n.max, n.min); \
    BindTo(*m##n##S, wxEVT_SLIDER, &Editor::On##n##Slider);

        SpinSlider(RoomSize,       XXO("&Room Size (%):"))
        SpinSlider(PreDelay,       XXO("&Pre-delay (ms):"))
        SpinSlider(Reverberance,   XXO("Rever&berance (%):"))
        SpinSlider(HfDamping,      XXO("Da&mping (%):"))
        SpinSlider(ToneLow,        XXO("Tone &Low (%):"))
        SpinSlider(ToneHigh,       XXO("Tone &High (%):"))
        SpinSlider(WetGain,        XXO("Wet &Gain (dB):"))
        SpinSlider(DryGain,        XXO("Dr&y Gain (dB):"))
        SpinSlider(StereoWidth,    XXO("Stereo Wid&th (%):"))

#undef SpinSlider
    }
    S.EndMultiColumn();

    S.StartHorizontalLay(wxCENTER, false);
    {
        mWetOnlyC
            =S.AddCheckBox(XXO("Wet O&nly"), WetOnly.def);
        BindTo(*mWetOnlyC, wxEVT_CHECKBOX, &Editor::OnCheckbox);
    }
    S.EndHorizontalLay();
}

bool EffectReverb::Editor::UpdateUI()
{
    // get the settings from the MessageBuffer and write them to our local copy
    mSettings = GetSettings(mAccess.Get());

    auto& rs = mSettings;

#define SetSpinSlider(n) \
    m##n##S->SetValue((int)rs.m##n); \
    m##n##T->SetValue(wxString::Format(wxT("%d"), (int)rs.m##n));

    SetSpinSlider(RoomSize);
    SetSpinSlider(PreDelay);
    SetSpinSlider(Reverberance);
    SetSpinSlider(HfDamping);
    SetSpinSlider(ToneLow);
    SetSpinSlider(ToneHigh);
    SetSpinSlider(WetGain);
    SetSpinSlider(DryGain);
    SetSpinSlider(StereoWidth);

#undef SetSpinSlider

    mWetOnlyC->SetValue((int)rs.mWetOnly);

    return true;
}

#define SpinSliderHandlers(n) \
    void EffectReverb::Editor::On##n##Slider(wxCommandEvent & evt) \
    { \
        if (mProcessingEvent) return; \
        mProcessingEvent = true; \
        m##n##T->SetValue(wxString::Format(wxT("%d"), evt.GetInt())); \
        mProcessingEvent = false; \
        ValidateUI(); \
        Publish(EffectSettingChanged {}); \
    } \
    void EffectReverb::Editor::On##n##Text(wxCommandEvent & evt) \
    { \
        if (mProcessingEvent) return; \
        mProcessingEvent = true; \
        m##n##S->SetValue(std::clamp<long>(evt.GetInt(), n.min, n.max)); \
        mProcessingEvent = false; \
        ValidateUI(); \
        Publish(EffectSettingChanged {}); \
    }

SpinSliderHandlers(RoomSize)
SpinSliderHandlers(PreDelay)
SpinSliderHandlers(Reverberance)
SpinSliderHandlers(HfDamping)
SpinSliderHandlers(ToneLow)
SpinSliderHandlers(ToneHigh)
SpinSliderHandlers(WetGain)
SpinSliderHandlers(DryGain)
SpinSliderHandlers(StereoWidth)

void EffectReverb::Editor::OnCheckbox(wxCommandEvent& evt)
{
    ValidateUI();
    Publish(EffectSettingChanged {});
}

#undef SpinSliderHandlers
