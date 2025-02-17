/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Effect.h"

#include <wx/log.h>

#include "internal/PlugFrame.h"
#include "internal/ConnectionProxy.h"

#include "SelectFile.h"

#include "ShuttleGui.h"

#include "VST3ParametersWindow.h"
#include "VST3OptionsDialog.h"
#include "VST3Wrapper.h"

#include "ConfigInterface.h"
#include "VST3Instance.h"
#include "VST3Editor.h"
#include "VST3EffectsModule.h"

VST3Effect::~VST3Effect()
{
    using namespace Steinberg;

    CloseUI();
}

int VST3Effect::ShowClientInterface(const EffectPlugin&,
                                    wxWindow& parent, wxDialog& dialog,
                                    EffectEditor* pEditor, bool forceModal) const
{
#ifdef __WXMSW__
    if (pEditor->IsGraphicalUI()) {
        //Not all platforms support window style change.
        //Plugins that support resizing provide their own handles,
        //which may overlap with system handle. Not all plugins
        //support free sizing (e.g. fixed steps or fixed ratio)
        dialog.SetWindowStyle(dialog.GetWindowStyle() & ~(wxRESIZE_BORDER | wxMAXIMIZE_BOX));
    }
#endif

    if (forceModal) {
        return dialog.ShowModal();
    }

    dialog.Show();
    return 0;
}

std::unique_ptr<EffectEditor> VST3Effect::PopulateUI(const EffectPlugin&,
                                                     ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
                                                     const EffectOutputs*) const
{
    bool useGUI { true };
    GetConfig(*this, PluginSettings::Shared, wxT("Options"),
              wxT("UseGUI"),
              useGUI,
              useGUI);

    const auto vst3instance = dynamic_cast<VST3Instance*>(&instance);

    return std::make_unique<VST3Editor>(S.GetParent(),
                                        vst3instance->GetWrapper(), *this, GetType(), access, useGUI);
}

std::unique_ptr<EffectEditor> VST3Effect::MakeEditor(
    ShuttleGui&, EffectInstance&, EffectSettingsAccess&,
    const EffectOutputs*) const
{
    //! Will not come here because Effect::PopulateUI is overridden
    assert(false);
    return nullptr;
}

void VST3Effect::ExportPresets(
    const EffectPlugin&, const EffectSettings& settings) const
{
    using namespace Steinberg;

    const auto path = SelectFile(FileNames::Operation::Presets,
                                 XO("Save VST3 Preset As:"),
                                 wxEmptyString,
                                 wxEmptyString,
                                 wxT(".vstpreset"),
    {
        { XO("VST3 preset file"), { wxT("vstpreset") }, true }
    },
                                 wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                                 NULL);

    if (path.empty()) {
        return;
    }

    auto wrapper = std::make_unique<VST3Wrapper>(*mModule, mEffectClassInfo);
    wrapper->InitializeComponents();

    auto dummy = EffectSettings { settings };
    wrapper->FetchSettings(dummy);
    wrapper->SavePresetToFile(path);
}

OptionalMessage VST3Effect::ImportPresets(
    const EffectPlugin&, EffectSettings& settings) const
{
    using namespace Steinberg;

    const auto path = SelectFile(FileNames::Operation::Presets,
                                 XO("Load VST3 preset:"),
                                 wxEmptyString,
                                 wxEmptyString,
                                 wxT(".vstpreset"),
    {
        { XO("VST3 preset file"), { wxT("vstpreset") }, true }
    },
                                 wxFD_OPEN | wxRESIZE_BORDER,
                                 nullptr
                                 );
    if (path.empty()) {
        return {}
    }

    LoadPreset(path, settings);

    return { nullptr };
}

void VST3Effect::ShowOptions(const EffectPlugin&) const
{
    VST3OptionsDialog { *this }.ShowModal();
}

// Inject factory hook to make VST3Effect capable of UI
static VST3EffectsModule::Factory::SubstituteInUnique<VST3Effect> scope;
