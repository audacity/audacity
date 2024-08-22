/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffects.h"

#include <QtQml>

#include "libraries/lib-module-manager/PluginManager.h"

#include "libraries/lib-effects/LoadEffects.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "amplify/amplifyeffect.h"
#include "amplify/amplifyviewmodel.h"

#include "tonegen/chirpeffect.h"
#include "tonegen/toneeffect.h"

#include "log.h"

using namespace au::effects;

void BuiltinEffects::init()
{
    static BuiltinEffectsModule::Registration< au::effects::AmplifyEffect > regAmplify;
    qmlRegisterType<AmplifyViewModel>("Audacity.Effects", 1, 0, "AmplifyViewModel");

    static BuiltinEffectsModule::Registration< au::effects::ChirpEffect > regChirp;
    static BuiltinEffectsModule::Registration< au::effects::ToneEffect > regTone;
}

EffectMetaList BuiltinEffects::effectMetaList() const
{
    EffectMetaList list;

    const auto range = PluginManager::Get().PluginsOfType(PluginTypeEffect);
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

        if (desc.GetSymbol() == AmplifyEffect::Symbol) {
            EffectMeta meta = AmplifyEffect::meta();
            meta.id = au3::wxToSting(desc.GetID());
            list.push_back(std::move(meta));
        } else if (desc.GetSymbol() == ChirpEffect::Symbol) {
            EffectMeta meta = ChirpEffect::meta();
            meta.id = au3::wxToSting(desc.GetID());
            list.push_back(std::move(meta));
        } else if (desc.GetSymbol() == ToneEffect::Symbol) {
            EffectMeta meta = ToneEffect::meta();
            meta.id = au3::wxToSting(desc.GetID());
            list.push_back(std::move(meta));
        }
    }

    return list;
}
