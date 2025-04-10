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
#include "dtmfgen/dtmfgenerator.h"
#include "dtmfgen/dtmfviewmodel.h"
#include "silencegen/silencegenerator.h"
#include "silencegen/silenceviewmodel.h"
#include "noisegen/noisegenerator.h"
#include "noisegen/noiseviewmodel.h"
#include "fade/fadeeffect.h"
#include "invert/inverteffect.h"
#include "reverse/reverseeffect.h"
#include "repair/repaireffect.h"
#include "log.h"

#include <algorithm>

using namespace au::effects;

namespace {
enum class EffectCategoryId {
    None,
    VolumeAndCompression,
    Fading,
    PitchAndTempo,
    EqAndFilters,
    NoiseRemovalAndRepair,
    DelayAndReverb,
    DistortionAndModulation,
    Special,
    Legacy,
};

muse::String categoryIdString(EffectCategoryId category)
{
    switch (category) {
    case EffectCategoryId::None:
        return muse::String{ "" };
    case EffectCategoryId::VolumeAndCompression:
        return muse::String{ "Volume and Compression" };
    case EffectCategoryId::Fading:
        return muse::String{ "Fading" };
    case EffectCategoryId::PitchAndTempo:
        return muse::String{ "Pitch and Tempo" };
    case EffectCategoryId::EqAndFilters:
        return muse::String{ "EQ and Filters" };
    case EffectCategoryId::NoiseRemovalAndRepair:
        return muse::String{ "Noise Removal and Repair" };
    case EffectCategoryId::DelayAndReverb:
        return muse::String{ "Delay and Reverb" };
    case EffectCategoryId::DistortionAndModulation:
        return muse::String{ "Distortion and Modulation" };
    case EffectCategoryId::Special:
        return muse::String{ "Special" };
    case EffectCategoryId::Legacy:
        return muse::String{ "Legacy" };
    default:
        assert(false);
        return muse::String{ "" };
    }
}
} // namespace

void BuiltinEffectsRepository::preInit()
{
    static BuiltinEffectsModule::Registration< FadeInEffect > regFadeIn;
    static BuiltinEffectsModule::Registration< FadeOutEffect > regFadeOut;
    static BuiltinEffectsModule::Registration< InvertEffect > regInvert;
    static BuiltinEffectsModule::Registration< Repair > regRepair;
    static BuiltinEffectsModule::Registration< ReverseEffect > regReverse;
    static BuiltinEffectsModule::Registration< AmplifyEffect > regAmplify;
    static BuiltinEffectsModule::Registration< ChirpEffect > regChirp;
    static BuiltinEffectsModule::Registration< ToneEffect > regTone;
    static BuiltinEffectsModule::Registration< ReverbEffect > regReverb;
    static BuiltinEffectsModule::Registration< SilenceGenerator > regSilence;
    static BuiltinEffectsModule::Registration< NoiseGenerator > regNoise;
    static BuiltinEffectsModule::Registration< DtmfGenerator > regDtmf;
}

void BuiltinEffectsRepository::init()
{
    updateEffectMetaList();
}

void BuiltinEffectsRepository::updateEffectMetaList()
{
    // For now, this method is called only once, so there is yet no need to clear the list and unregister the views.
    // It will have to be implemented when we provide the user the possibility of rescanning the effects, though.
    assert(m_metas.empty());

    auto regView = [this](const ::ComponentInterfaceSymbol& symbol, const muse::String& url) {
        effectsViewRegister()->regUrl(au3::wxToString(symbol.Internal()), url);
    };

    auto regMeta
        = [this](const ::PluginDescriptor& desc, const muse::String& title, const muse::String& description, EffectCategoryId category,
                 bool supportsMultipleClipSelection) {
        EffectMeta meta;
        meta.id = au3::wxToString(desc.GetID());
        meta.family = EffectFamily::Builtin;
        meta.category = categoryIdString(category);
        meta.title = title;
        meta.description = description;
        meta.isRealtimeCapable = desc.IsEffectRealtime();
        meta.supportsMultipleClipSelection = supportsMultipleClipSelection;
        meta.vendor = "Audacity";
        meta.path = desc.GetPath();

        switch (desc.GetEffectType()) {
        case EffectTypeGenerate:
            meta.type = EffectType::Generator;
            break;
        case EffectTypeProcess:
            meta.type = EffectType::Processor;
            break;
        case EffectTypeAnalyze:
            meta.type = EffectType::Analyzer;
            break;
        default:
            assert(false);
        }

        m_metas.insert({ desc.GetSymbol(), meta });
    };

    // General
    qmlRegisterType<GeneralViewModel>("Audacity.Effects", 1, 0, "GeneralViewModel");
    effectsViewRegister()->setDefaultUrl(u"qrc:/general/GeneralEffectView.qml");

    for (const PluginDescriptor& desc : PluginManager::Get().PluginsOfType(PluginTypeEffect)) {
        const auto& symbol = desc.GetSymbol();
        if (symbol == AmplifyEffect::Symbol) {
            qmlRegisterType<AmplifyViewModel>("Audacity.Effects", 1, 0, "AmplifyViewModel");
            regView(AmplifyEffect::Symbol, u"qrc:/amplify/AmplifyView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Amplify"),
                    muse::mtrc("effects", "Increases or decreases the volume of the audio you have selected"),
                    EffectCategoryId::VolumeAndCompression,
                    false
                    );
        } else if (symbol == FadeInEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Fade In"),
                    muse::mtrc("effects", "Applies a linear fade-in to the selected audio"),
                    EffectCategoryId::Fading,
                    true
                    );
        } else if (symbol == FadeOutEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Fade Out"),
                    muse::mtrc("effects", "Applies a linear fade-out to the selected audio"),
                    EffectCategoryId::Fading,
                    true
                    );
        } else if (symbol == InvertEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Invert"),
                    muse::mtrc("effects", "Flips the audio samples upside-down, reversing their polarity"),
                    EffectCategoryId::Special,
                    true
                    );
        } else if (symbol == Repair::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Repair"),
                    muse::mtrc("effects", "Sets the peak amplitude of a one or more tracks"),
                    EffectCategoryId::NoiseRemovalAndRepair,
                    false
                    );
        } else if (symbol == ReverseEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Reverse"),
                    muse::mtrc("effects", "Reverses the selected audio"),
                    EffectCategoryId::Special,
                    true
                    );
        } else if (symbol == ChirpEffect::Symbol) {
            regView(ChirpEffect::Symbol, u"qrc:/tonegen/ChirpView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Chirp"),
                    muse::mtrc("effects", "Generates an ascending or descending tone of one of four types"),
                    EffectCategoryId::None,
                    false
                    );
        } else if (symbol == ToneEffect::Symbol) {
            qmlRegisterType<ToneViewModel>("Audacity.Effects", 1, 0, "ToneViewModel");
            regView(ToneEffect::Symbol, u"qrc:/tonegen/ToneView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Tone"),
                    muse::mtrc("effects", "Generates a constant frequency tone of one of four types"),
                    EffectCategoryId::None,
                    false
                    );
        } else if (symbol == ReverbEffect::Symbol) {
            qmlRegisterType<ReverbViewModel>("Audacity.Effects", 1, 0, "ReverbViewModel");
            regView(ReverbEffect::Symbol, u"qrc:/reverb/ReverbView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Reverb"),
                    muse::mtrc("effects", "Reverb effect"),
                    EffectCategoryId::DelayAndReverb,
                    true
                    );
        } else if (symbol == NoiseGenerator::Symbol) {
            qmlRegisterType<NoiseViewModel>("Audacity.Effects", 1, 0, "NoiseViewModel");
            regView(NoiseGenerator::Symbol, u"qrc:/noisegen/NoiseView.qml");
            regMeta(desc,
                    muse::mtrc("effects/noise", "Noise"),
                    muse::mtrc("effects/noise", "Generates noise"),
                    EffectCategoryId::None,
                    false
                    );
        } else if (symbol == DtmfGenerator::Symbol) {
            qmlRegisterType<DtmfViewModel>("Audacity.Effects", 1, 0, "DtmfViewModel");
            regView(DtmfGenerator::Symbol, u"qrc:/dtmfgen/DtmfView.qml");
            regMeta(desc,
                    muse::mtrc("effects/dtmf", "DTMF Tones"),
                    muse::mtrc("effects/dtmf", "Generates DTMF signal"),
                    EffectCategoryId::None,
                    false
                    );
        } else if (symbol == SilenceGenerator::Symbol) {
            qmlRegisterType<SilenceViewModel>("Audacity.Effects", 1, 0, "SilenceViewModel");
            regView(SilenceGenerator::Symbol, u"qrc:/silencegen/SilenceView.qml");
            regMeta(desc,
                    muse::mtrc("effects/silence", "Silence"),
                    muse::mtrc("effects/silence", "Generates silence"),
                    EffectCategoryId::None,
                    false
                    );
        } else {
            LOGW() << "effect not found for symbol: " << au3::wxToStdSting(symbol.Internal());
        }
    }

    m_effectMetaListUpdated.notify();
}

muse::async::Notification BuiltinEffectsRepository::effectMetaListUpdated() const
{
    return m_effectMetaListUpdated;
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
    EffectMetaList list(m_metas.size());
    std::transform(m_metas.begin(), m_metas.end(), list.begin(), [](const auto& pair) { return pair.second; });
    return list;
}
