/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffects.h"

#include <QtQml>

#include "global/translation.h"

#include "libraries/lib-module-manager/PluginManager.h"

#include "libraries/lib-effects/LoadEffects.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "general/generalviewmodel.h"

#include "amplify/amplifyeffect.h"
#include "amplify/amplifyviewmodel.h"

#include "tonegen/chirpeffect.h"
#include "tonegen/toneeffect.h"

#include "log.h"

using namespace au::effects;

static EffectMeta effectMeta(const ComponentInterfaceSymbol& symbol)
{
    EffectMeta meta;
    meta.categoryId = BUILTIN_CATEGORY_ID;
    if (symbol == AmplifyEffect::Symbol) {
        meta.title = muse::mtrc("effects", "Amplify");
        meta.description = muse::mtrc("effects", "Increases or decreases the volume of the audio you have selected");
    } else if (symbol == ChirpEffect::Symbol) {
        meta.title = muse::mtrc("effects", "Chirp");
        meta.description = muse::mtrc("effects", "Generates an ascending or descending tone of one of four types");
    } else if (symbol == ToneEffect::Symbol) {
        meta.title = muse::mtrc("effects", "Tone");
        meta.description = muse::mtrc("effects", "Generates a constant frequency tone of one of four types");
    }

    return meta;
}

void BuiltinEffects::init()
{
    auto regView = [](const ::ComponentInterfaceSymbol& symbol, const muse::String& url) {
        BuiltinEffects::effectsViewRegister()->regUrl(au3::wxToSting(symbol.Internal()), url);
    };

    // General
    qmlRegisterType<GeneralViewModel>("Audacity.Effects", 1, 0, "GeneralViewModel");
    effectsViewRegister()->setDefaultUrl(u"qrc:/builtin/general/GeneralEffectView.qml");

    // Specific
    static BuiltinEffectsModule::Registration< AmplifyEffect > regAmplify;
    qmlRegisterType<AmplifyViewModel>("Audacity.Effects", 1, 0, "AmplifyViewModel");
    regView(AmplifyEffect::Symbol, u"qrc:/builtin/amplify/AmplifyView.qml");

    static BuiltinEffectsModule::Registration< ChirpEffect > regChirp;
    //regView(ChirpEffect::Symbol, u"qrc:/builtin/tonegen/ChirpView.qml");

    static BuiltinEffectsModule::Registration< ToneEffect > regTone;
    //regView(ToneEffect::Symbol, u"qrc:/builtin/tonegen/ToneView.qml");
}

EffectMetaList BuiltinEffects::effectMetaList() const
{
    EffectMetaList list;

    const auto range = PluginManager::Get().PluginsOfType(PluginTypeEffect);
    for (const PluginDescriptor& desc : range) {
        LOGDA() << " ID: " << au3::wxToStdSting(desc.GetID())
                << ", Symbol: " << au3::wxToStdSting(desc.GetSymbol().Internal());

        EffectMeta meta = effectMeta(desc.GetSymbol());
        meta.id = au3::wxToSting(desc.GetID());
        list.push_back(std::move(meta));
    }

    return list;
}
