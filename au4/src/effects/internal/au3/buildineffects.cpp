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

#include "../../effectstypes.h"

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

    static EffectMeta meta(const ::PluginID& pluginId)
    {
        EffectMeta meta;
        meta.id = au3::wxToSting(pluginId);
        meta.title = muse::mtrc("effects", "Amplify");
        meta.categoryId = BUILDIN_CATEGORY_ID;
        meta.qmlUrl = "qrc:/qml/Audacity/Effects/Buildin/AmplifyEffect.qml";

        return meta;
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

    static EffectMeta meta(const ::PluginID& pluginId)
    {
        EffectMeta meta;
        meta.id = au3::wxToSting(pluginId);
        meta.title = muse::mtrc("effects", "Chirp");
        meta.categoryId = BUILDIN_CATEGORY_ID;
        meta.qmlUrl = "qrc:/qml/Audacity/Effects/Buildin/ChirpEffect.qml";

        return meta;
    }
};

class EffectTone final : public EffectToneGen
{
public:
    static const ComponentInterfaceSymbol Symbol;

    EffectTone()
        : EffectToneGen{false} {}
    ~EffectTone() override = default;

    static EffectMeta meta(const ::PluginID& pluginId)
    {
        EffectMeta meta;
        meta.id = au3::wxToSting(pluginId);
        meta.title = muse::mtrc("effects", "Tone");
        meta.categoryId = BUILDIN_CATEGORY_ID;
        meta.qmlUrl = "qrc:/qml/Audacity/Effects/Buildin/ToneEffect.qml";

        return meta;
    }
};

const ComponentInterfaceSymbol EffectChirp::Symbol{ XO("Chirp") };
const ComponentInterfaceSymbol EffectTone::Symbol{ XO("Tone") };

ComponentInterfaceSymbol EffectToneGen::GetSymbol() const
{
    return mChirp ? EffectChirp::Symbol : EffectTone::Symbol;
}
}

// BuildInEffects ----

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
        LOGDA() << " ID: " << au3::wxToStdSting(desc.GetID())
                << ", Symbol: " << au3::wxToStdSting(desc.GetSymbol().Internal());

        //! NOTE Effects become plugins, with their interface...
        //! The plugin interface for forming the meta effect is not enough for us,
        //! for example, we need a URL to the Qml representation
        //! And we can't just add virtual methods to the effect to return the meta.
        //!
        //! And we can't just make a meta list from the built-in plugins we know about,
        //! so the plugin ID is used as the effect ID.
        //!
        //! So here is such a design, perhaps we can do it better somehow

        if (desc.GetSymbol() == EffectAmplify::Symbol) {
            list.push_back(EffectAmplify::meta(desc.GetID()));
        } else if (desc.GetSymbol() == EffectChirp::Symbol) {
            list.push_back(EffectChirp::meta(desc.GetID()));
        } else if (desc.GetSymbol() == EffectTone::Symbol) {
            list.push_back(EffectTone::meta(desc.GetID()));
        }
    }

    return list;
}
