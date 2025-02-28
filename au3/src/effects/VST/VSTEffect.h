/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_VST_EFFECT__
#define __AUDACITY_VST_EFFECT__

#include "VSTEffectBase.h"
#include "../StatelessPerTrackEffect.h"

#include "SampleFormat.h"

#include <optional>
#include <atomic>

typedef intptr_t (* dispatcherFn)(AEffect* effect, int opCode, int index, intptr_t value, void* ptr, float opt);

typedef void (* processFn)(AEffect* effect, float** inputs, float** outputs, int sampleframes);

typedef void (* setParameterFn)(AEffect* effect, int index, float parameter);

typedef float (* getParameterFn)(AEffect* effect, int index);

typedef AEffect*(* vstPluginMain)(audioMasterCallback audioMaster);

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////

class VSTEditor;

class VSTEffect final : public StatelessEffectUIServices, public VSTEffectBase
{
public:
    using VSTEffectBase::VSTEffectBase;
    ~VSTEffect() override;

private:
    int ShowClientInterface(const EffectPlugin& plugin, wxWindow& parent, wxDialog& dialog, EffectEditor* pEditor, bool forceModal)
    const override;

    std::unique_ptr<EffectEditor> PopulateUI(const EffectPlugin& plugin, ShuttleGui& S, EffectInstance& instance,
                                             EffectSettingsAccess& access, const EffectOutputs* pOutputs) const override;

    void ExportPresets(
        const EffectPlugin& plugin, const EffectSettings& settings)
    const override;

    OptionalMessage ImportPresets(
        const EffectPlugin& plugin, EffectSettings& settings) const override;

    // Non-const and non-virtual function:
    OptionalMessage ImportPresetsNC(EffectSettings& settings);
    void ShowOptions(const EffectPlugin& plugin) const override;

    //! Will never be called
    virtual std::unique_ptr<EffectEditor> MakeEditor(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) const final;
};

#endif
