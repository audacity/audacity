/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsrepository.h"

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
#include "reverb/reverbeffect.h"
#include "reverb/reverbviewmodel.h"
#include "tonegen/toneviewmodel.h"
#include "silencegen/silenceeffect.h"
#include "silencegen/silenceviewmodel.h"

#include "log.h"

using namespace au::effects;

void BuiltinEffectsRepository::init()
{
    auto regView = [this](const ::ComponentInterfaceSymbol& symbol, const muse::String& url) {
        effectsViewRegister()->regUrl(au3::wxToString(symbol.Internal()), url);
    };

    auto regMeta = [this](const ::ComponentInterfaceSymbol& symbol, const muse::String& title, const muse::String& description) {
        EffectMeta meta;
        meta.categoryId = BUILTIN_CATEGORY_ID;
        meta.title = title;
        meta.description = description;
        m_metas.insert({ symbol, meta });
    };

    // General
    qmlRegisterType<GeneralViewModel>("Audacity.Effects", 1, 0, "GeneralViewModel");
    effectsViewRegister()->setDefaultUrl(u"qrc:/general/GeneralEffectView.qml");

    // Specific
    static BuiltinEffectsModule::Registration< AmplifyEffect > regAmplify;
    qmlRegisterType<AmplifyViewModel>("Audacity.Effects", 1, 0, "AmplifyViewModel");
    regView(AmplifyEffect::Symbol, u"qrc:/amplify/AmplifyView.qml");
    regMeta(AmplifyEffect::Symbol,
            muse::mtrc("effects", "Amplify"),
            muse::mtrc("effects", "Increases or decreases the volume of the audio you have selected")
            );

    static BuiltinEffectsModule::Registration< ChirpEffect > regChirp;
    regView(ChirpEffect::Symbol, u"qrc:/tonegen/ChirpView.qml");
    regMeta(ChirpEffect::Symbol,
            muse::mtrc("effects", "Chirp"),
            muse::mtrc("effects", "Generates an ascending or descending tone of one of four types")
            );

    static BuiltinEffectsModule::Registration< SilenceEffect > regSilence;
    qmlRegisterType<SilenceViewModel>("Audacity.Effects", 1, 0, "SilenceViewModel");
    regView(SilenceEffect::Symbol, u"qrc:/silencegen/SilenceView.qml");
    regMeta(SilenceEffect::Symbol,
            muse::mtrc("effects", "Silence"),
            muse::mtrc("effects", "Generates silence")
            );

    static BuiltinEffectsModule::Registration< ToneEffect > regTone;
    qmlRegisterType<ToneViewModel>("Audacity.Effects", 1, 0, "ToneViewModel");
    regView(ToneEffect::Symbol, u"qrc:/tonegen/ToneView.qml");
    regMeta(ToneEffect::Symbol,
            muse::mtrc("effects", "Tone"),
            muse::mtrc("effects", "Generates a constant frequency tone of one of four types")
            );

    static BuiltinEffectsModule::Registration< ReverbEffect > regReverb;
    qmlRegisterType<ReverbViewModel>("Audacity.Effects", 1, 0, "ReverbViewModel");
    regView(ReverbEffect::Symbol, u"qrc:/reverb/ReverbView.qml");
    regMeta(ReverbEffect::Symbol,
            muse::mtrc("effects", "Reverb"),
            muse::mtrc("effects", "Reverb effect")
            );
}

EffectMeta BuiltinEffectsRepository::effectMeta(const ComponentInterfaceSymbol& symbol) const
{
    auto it = m_metas.find(symbol);
    if (it == m_metas.end()) {
        LOGW() << "not found effect meta for symbol: " << au3::wxToStdSting(symbol.Internal());
        return EffectMeta();
    }
    return it->second;
}

EffectMetaList BuiltinEffectsRepository::effectMetaList() const
{
    EffectMetaList list;

    const auto range = PluginManager::Get().PluginsOfType(PluginTypeEffect);
    for (const PluginDescriptor& desc : range) {
        LOGD() << " ID: " << au3::wxToStdSting(desc.GetID())
               << ", Symbol: " << au3::wxToStdSting(desc.GetSymbol().Internal());

        EffectMeta meta = effectMeta(desc.GetSymbol());
        meta.id = au3::wxToString(desc.GetID());
        list.push_back(std::move(meta));
    }

    return list;
}
