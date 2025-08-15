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
#include "loudness/normalizeloudnesseffect.h"
#include "loudness/normalizeloudnessviewmodel.h"
#include "clickremoval/clickremovaleffect.h"
#include "clickremoval/clickremovalviewmodel.h"
#include "compressor/compressoreffect.h"
#include "compressor/compressorviewmodel.h"
#include "limiter/limitereffect.h"
#include "limiter/limiterviewmodel.h"
#include "normalize/normalizeeffect.h"
#include "normalize/normalizeviewmodel.h"
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
#include "noisereduction/noisereductioneffect.h"
#include "noisereduction/noisereductionviewmodel.h"
#include "fade/fadeeffect.h"
#include "graphiceq/graphiceq.h"
#include "graphiceq/graphiceqbandsmodel.h"
#include "graphiceq/graphiceqviewmodel.h"
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
        // Maybe a temporary solution to generator menu organization:
        // since generators don't have a category, with this they'll end up in the Audacity "category".
        // When we implement Nyquist support, though, either we create the
        // "Nyquist" category and are happy with that, or we
        // will have to ask our designers to specify organization of generators
        // (and analyzers).
        return muse::String{ "Audacity" };
    case EffectCategoryId::VolumeAndCompression:
        return muse::String{ "Volume and compression" };
    case EffectCategoryId::Fading:
        return muse::String{ "Fading" };
    case EffectCategoryId::PitchAndTempo:
        return muse::String{ "Pitch and tempo" };
    case EffectCategoryId::EqAndFilters:
        return muse::String{ "EQ and filters" };
    case EffectCategoryId::NoiseRemovalAndRepair:
        return muse::String{ "Noise removal and repair" };
    case EffectCategoryId::DelayAndReverb:
        return muse::String{ "Delay and reverb" };
    case EffectCategoryId::DistortionAndModulation:
        return muse::String{ "Distortion and modulation" };
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
    static BuiltinEffectsModule::Registration< NormalizeLoudnessEffect > regLoudness;
    static BuiltinEffectsModule::Registration< GraphicEq > regGraphicEq;
    static BuiltinEffectsModule::Registration< ClickRemovalEffect > regClickRemoval;
    static BuiltinEffectsModule::Registration< NormalizeEffect > regNormalize;
    static BuiltinEffectsModule::Registration< ChirpEffect > regChirp;
    static BuiltinEffectsModule::Registration< ToneEffect > regTone;
    static BuiltinEffectsModule::Registration< ReverbEffect > regReverb;
    static BuiltinEffectsModule::Registration< SilenceGenerator > regSilence;
    static BuiltinEffectsModule::Registration< NoiseGenerator > regNoise;
    static BuiltinEffectsModule::Registration< NoiseReductionEffect > regNoiseReduction;
    static BuiltinEffectsModule::Registration< DtmfGenerator > regDtmf;
    static BuiltinEffectsModule::Registration< CompressorEffect > regCompressor;
    static BuiltinEffectsModule::Registration< LimiterEffect > regLimiter;
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
        } else if (symbol == NormalizeLoudnessEffect::Symbol) {
            qmlRegisterType<NormalizeLoudnessViewModel>("Audacity.Effects", 1, 0, "NormalizeLoudnessViewModel");
            regView(NormalizeLoudnessEffect::Symbol, u"qrc:/loudness/NormalizeLoudnessView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Loudness normalization"),
                    muse::mtrc("effects", "Sets the loudness of one or more tracks"),
                    EffectCategoryId::VolumeAndCompression,
                    true
                    );
        } else if (symbol == GraphicEq::Symbol) {
            qmlRegisterType<GraphicEqViewModel>("Audacity.Effects", 1, 0, "GraphicEqViewModel");
            qmlRegisterType<GraphicEqBandsModel>("Audacity.Effects", 1, 0, "GraphicEqBandsModel");
            regView(GraphicEq::Symbol, u"qrc:/graphiceq/GraphicEqView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Graphic EQ"),
                    muse::mtrc("effects", "Adjusts the balance between frequency components"),
                    EffectCategoryId::EqAndFilters,
                    true
                    );
        } else if (symbol == ClickRemovalEffect::Symbol) {
            qmlRegisterType<ClickRemovalViewModel>("Audacity.Effects", 1, 0, "ClickRemovalViewModel");
            regView(ClickRemovalEffect::Symbol, u"qrc:/clickremoval/ClickRemovalView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Click Removal"),
                    muse::mtrc("effects", "Click Removal is designed to remove clicks on audio tracks"),
                    EffectCategoryId::NoiseRemovalAndRepair,
                    true
                    );
        } else if (symbol == CompressorEffect::Symbol) {
            qmlRegisterType<CompressorViewModel>("Audacity.Effects", 1, 0, "CompressorViewModel");
            qmlRegisterType<CompressorSettingModel>("Audacity.Effects", 1, 0, "CompressorSettingModel");
            regView(CompressorEffect::Symbol, u"qrc:/compressor/CompressorView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Compressor"),
                    muse::mtrc("effects", "Reduces \"dynamic range\", or differences between loud and quiet parts"),
                    EffectCategoryId::VolumeAndCompression,
                    true
                    );
        } else if (symbol == LimiterEffect::Symbol) {
            qmlRegisterType<LimiterViewModel>("Audacity.Effects", 1, 0, "LimiterViewModel");
            qmlRegisterType<LimiterSettingModel>("Audacity.Effects", 1, 0, "LimiterSettingModel");
            regView(LimiterEffect::Symbol, u"qrc:/limiter/LimiterView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Limiter"),
                    muse::mtrc("effects", "Augments loudness while minimizing distortion"),
                    EffectCategoryId::VolumeAndCompression,
                    true
                    );
        } else if (symbol == NormalizeEffect::Symbol) {
            qmlRegisterType<NormalizeViewModel>("Audacity.Effects", 1, 0, "NormalizeViewModel");
            regView(NormalizeEffect::Symbol, u"qrc:/normalize/NormalizeView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Normalize"),
                    muse::mtrc("effects", "Sets the peak amplitude of a one or more tracks"),
                    EffectCategoryId::VolumeAndCompression,
                    false
                    );
        } else if (symbol == FadeInEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Fade in"),
                    muse::mtrc("effects", "Applies a linear fade-in to the selected audio"),
                    EffectCategoryId::Fading,
                    true
                    );
        } else if (symbol == FadeOutEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Fade out"),
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
        } else if (symbol == NoiseReductionEffect::Symbol) {
            qmlRegisterType<NoiseReductionViewModel>("Audacity.Effects", 1, 0, "NoiseReductionViewModel");
            regView(NoiseReductionEffect::Symbol, u"qrc:/noisereduction/NoiseReductionView.qml");
            regMeta(desc,
                    muse::mtrc("effects/noisereduction", "Noise Reduction"),
                    muse::mtrc("effects/noisereduction", "Reduces noise in the audio"),
                    EffectCategoryId::NoiseRemovalAndRepair,
                    false
                    );
        } else if (symbol == DtmfGenerator::Symbol) {
            qmlRegisterType<DtmfViewModel>("Audacity.Effects", 1, 0, "DtmfViewModel");
            regView(DtmfGenerator::Symbol, u"qrc:/dtmfgen/DtmfView.qml");
            regMeta(desc,
                    muse::mtrc("effects/dtmf", "DTMF tones"),
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
