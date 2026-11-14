/**********************************************************************

  Audacity: A Digital Audio Editor

  Dominic Mazzoni
  Vaughan Johnson (dialog)

*******************************************************************//**

\class EffectEcho
\brief An Effect that causes an echo, variable delay and volume.

*//****************************************************************//**

\class EchoDialog
\brief EchoDialog used with EffectEcho

*//*******************************************************************/

#include "Echo.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "../widgets/valnum.h"

namespace {
BuiltinEffectsModule::Registration< EffectEcho > reg;
}

std::shared_ptr<EffectInstance> EffectEcho::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

struct EffectEcho::Editor : EffectEditor
{
    Editor(const EffectUIServices& services,
           EffectSettingsAccess& access, const EchoSettings& settings)
        : EffectEditor{services, access}
        , mSettings{settings}
    {}
    virtual ~Editor() = default;

    bool ValidateUI() override;
    bool UpdateUI() override;

    void PopulateOrExchange(ShuttleGui& S);

    EchoSettings mSettings;
};

std::unique_ptr<EffectEditor> EffectEcho::MakeEditor(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*) const
{
    auto& settings = access.Get();
    auto& myEffSettings = GetSettings(settings);
    auto result = std::make_unique<Editor>(*this, access, myEffSettings);
    result->PopulateOrExchange(S);
    return result;
}

void EffectEcho::Editor::PopulateOrExchange(ShuttleGui& S)
{
    auto& echoSettings = mSettings;

    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.Validator<FloatingPointValidator<double> >(
            3, &echoSettings.delay, NumValidatorStyle::NO_TRAILING_ZEROES,
            Delay.min, Delay.max)
        .AddTextBox(XXO("&Delay time (seconds):"), L"", 10);

        S.Validator<FloatingPointValidator<double> >(
            3, &echoSettings.decay, NumValidatorStyle::NO_TRAILING_ZEROES,
            Decay.min, Decay.max)
        .AddTextBox(XXO("D&ecay factor:"), L"", 10);
    }
    S.EndMultiColumn();
}

bool EffectEcho::Editor::ValidateUI()
{
    mAccess.ModifySettings
    (
        [this](EffectSettings& settings)
    {
        // pass back the modified settings to the MessageBuffer

        EffectEcho::GetSettings(settings) = mSettings;
        return nullptr;
    }
    );

    return true;
}

bool EffectEcho::Editor::UpdateUI()
{
    // get the settings from the MessageBuffer and write them to our local copy
    const auto& settings = mAccess.Get();

    mSettings = GetSettings(settings);

    return true;
}
