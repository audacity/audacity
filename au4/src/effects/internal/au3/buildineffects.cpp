/*
* Audacity: A Digital Audio Editor
*/
#include "buildineffects.h"

#include "global/translation.h"

#include "libraries/lib-builtin-effects/AmplifyBase.h"
#include "libraries/lib-builtin-effects/ToneGenBase.h"

#include "libraries/lib-module-manager/PluginManager.h"

#include "libraries/lib-effects/LoadEffects.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;

namespace au::effects {
//! NOTE I don't know yet how it will look in the end,
//! these are the first steps to register the effects and get the list
class EffectAmplify : public ::AmplifyBase
{
public:
    ComponentInterfaceSymbol GetSymbol() const override
    {
        return Symbol;
    }

    TranslatableString GetDescription() const override
    {
        return XO("Increases or decreases the volume of the audio you have selected");
    }

    ManualPageID ManualPage() const override
    {
        return L"Amplify";
    }
};

class EffectToneGen : public ::ToneGenBase
{
public:

    EffectToneGen(bool isChirp)
        : ToneGenBase{isChirp}
    {
    }

    ComponentInterfaceSymbol GetSymbol() const override;

    TranslatableString GetDescription() const override
    {
        return mChirp
               ? XO("Generates an ascending or descending tone of one of four types")
               : XO("Generates a constant frequency tone of one of four types");
    }

    ManualPageID ManualPage() const override
    {
        return mChirp ? L"Chirp" : L"Tone";
    }
};

class EffectChirp final : public EffectToneGen
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectChirp()
        : EffectToneGen{true} {}
    ~EffectChirp() override = default;
};

class EffectTone final : public EffectToneGen
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectTone()
        : EffectToneGen{false} {}
    ~EffectTone() override = default;
};

const ComponentInterfaceSymbol EffectChirp::Symbol{ XO("Chirp") };
const ComponentInterfaceSymbol EffectTone::Symbol{ XO("Tone") };

ComponentInterfaceSymbol EffectToneGen::GetSymbol() const
{
    return mChirp ? EffectChirp::Symbol : EffectTone::Symbol;
}
}

// BuildInEffects ----
static muse::String effectTitle(const ::ComponentInterfaceSymbol& symbol)
{
    //! NOTE This is necessary so that the translated names get into the translation system,
    //! we translate them and use them depending on the current language.

    if (symbol == EffectAmplify::Symbol) {
        return muse::mtrc("effects", "Amplify");
    } else if (symbol == EffectChirp::Symbol) {
        return muse::mtrc("effects", "Chirp");
    } else if (symbol == EffectTone::Symbol) {
        return muse::mtrc("effects", "Tone");
    }

    UNREACHABLE;
    return muse::String();
}

void BuildInEffects::init()
{
    static BuiltinEffectsModule::Registration< au::effects::EffectAmplify > regAmplify;
    static BuiltinEffectsModule::Registration< au::effects::EffectChirp > regChirp;
    static BuiltinEffectsModule::Registration< au::effects::EffectTone > regTone;
}

EffectMetaList BuildInEffects::effectMetaList() const
{
    EffectMetaList list;
    auto range = PluginManager::Get().PluginsOfType(PluginTypeEffect);
    for (const PluginDescriptor& desc : range) {
        // LOGDA() << " ID: " << au3::wxToStdSting(desc.GetID())
        //         << ", Symbol: " << au3::wxToStdSting(desc.GetSymbol().Internal());

        EffectMeta meta;
        meta.id = au3::wxToSting(desc.GetID());
        meta.title = effectTitle(desc.GetSymbol());

        list.push_back(std::move(meta));
    }

    return list;
}
