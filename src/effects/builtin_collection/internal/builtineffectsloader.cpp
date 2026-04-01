/*
* Audacity: A Digital Audio Editor
*/
#include "builtineffectsloader.h"

#include <QtQml>

#include "global/translation.h"
#include "global/log.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-effects/LoadEffects.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/internal/au3/au3effectsutils.h"
#include "effects/effects_base/internal/effectsutils.h"
#include "effects/effects_base/view/effectsviewutils.h"

#include "amplify/amplifyeffect.h"
#include "amplify/amplifyviewmodel.h"
#include "loudness/normalizeloudnesseffect.h"
#include "loudness/normalizeloudnessviewmodel.h"
#include "clickremoval/clickremovaleffect.h"
#include "clickremoval/clickremovalviewmodel.h"
#include "dynamics/timeline/meters/compressiondbmetermodel.h"
#include "dynamics/timeline/meters/outputdbmetermodel.h"
#include "dynamics/timeline/dynamicstimeline.h"
#include "dynamics/timeline/timelinesourcemodel.h"
#include "dynamics/timeline/stopwatch.h"
#include "dynamics/timeline/dynamicsplaystatemodel.h"
#include "dynamics/compressor/compressoreffect.h"
#include "dynamics/compressor/compressorviewmodel.h"
#include "dynamics/compressor/compressorsettingmodel.h"
#include "dynamics/limiter/limitereffect.h"
#include "dynamics/limiter/limiterviewmodel.h"
#include "dynamics/limiter/limitersettingmodel.h"
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
#include "removedcoffset/removedcoffseteffect.h"
#include "repair/repaireffect.h"
#include "truncatesilence/truncatesilenceeffect.h"
#include "truncatesilence/truncatesilenceviewmodel.h"
#if USE_SOUNDTOUCH
#include "changepitch/changepitcheffect.h"
#include "changepitch/changepitchviewmodel.h"
#endif

#include <algorithm>

using namespace au::effects;

void BuiltinEffectsLoader::preInit()
{
    static BuiltinEffectsModule::Registration< FadeInEffect > regFadeIn;
    static BuiltinEffectsModule::Registration< FadeOutEffect > regFadeOut;
    static BuiltinEffectsModule::Registration< InvertEffect > regInvert;
    static BuiltinEffectsModule::Registration< Repair > regRepair;
    static BuiltinEffectsModule::Registration< ReverseEffect > regReverse;
    static BuiltinEffectsModule::Registration< TruncateSilenceEffect > regTruncateSilence;
#if USE_SOUNDTOUCH
    static BuiltinEffectsModule::Registration< ChangePitchEffect > regChangePitch;
#endif
    static BuiltinEffectsModule::Registration< AmplifyEffect > regAmplify;
    static BuiltinEffectsModule::Registration< NormalizeLoudnessEffect > regLoudness;
    static BuiltinEffectsModule::Registration< GraphicEq > regGraphicEq;
    static BuiltinEffectsModule::Registration< ClickRemovalEffect > regClickRemoval;
    static BuiltinEffectsModule::Registration< NormalizeEffect > regNormalize;
    static BuiltinEffectsModule::Registration< RemoveDCOffsetEffect > regRemoveDCOffset;
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

void BuiltinEffectsLoader::init()
{
    auto regView = [this](const ::ComponentInterfaceSymbol& symbol, const muse::String& url) {
        builtinEffectsViewRegister()->regUrl(au3::wxToString(symbol.Internal()), url);
    };

    auto regMeta
        = [this](const ::PluginDescriptor& desc, const muse::String& title, const muse::String& description,
                 bool supportsMultipleClipSelection) {
        EffectMeta meta;
        meta.id = au3::wxToString(desc.GetID());
        meta.family = EffectFamily::Builtin;
        meta.category = utils::builtinEffectCategoryIdString(toAu4EffectCategory(desc.GetEffectGroup()));
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
        case EffectTypeTool:
            meta.type = EffectType::Tool;
            break;
        default:
            assert(false);
        }

        builtinEffectsRepository()->registerMeta(meta);
    };

    bool hasDynamicRangeProcessor = false;
    for (const PluginDescriptor& desc : PluginManager::Get().PluginsOfType(PluginTypeEffect)) {
        const auto& symbol = desc.GetSymbol();
        if (symbol == AmplifyEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(AmplifyViewModelFactory);
            regView(AmplifyEffect::Symbol, u"qrc:/amplify/AmplifyView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Amplify"),
                    muse::mtrc("effects", "Increases or decreases the volume of the audio you have selected"),
                    false
                    );
        } else if (symbol == NormalizeLoudnessEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NormalizeLoudnessViewModelFactory);
            regView(NormalizeLoudnessEffect::Symbol, u"qrc:/loudness/NormalizeLoudnessView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Loudness normalization"),
                    muse::mtrc("effects", "Sets the loudness of one or more tracks"),
                    true
                    );
        } else if (symbol == GraphicEq::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(GraphicEqViewModelFactory);
            qmlRegisterType<GraphicEqBandsModel>("Audacity.Effects", 1, 0, "GraphicEqBandsModel");
            regView(GraphicEq::Symbol, u"qrc:/graphiceq/GraphicEqView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Graphic EQ"),
                    muse::mtrc("effects", "Adjusts the balance between frequency components"),
                    true
                    );
        } else if (symbol == ClickRemovalEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ClickRemovalViewModelFactory);
            regView(ClickRemovalEffect::Symbol, u"qrc:/clickremoval/ClickRemovalView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Click removal"),
                    muse::mtrc("effects", "Click removal is designed to remove clicks on audio tracks"),
                    true
                    );
        } else if (symbol == CompressorEffect::Symbol) {
            hasDynamicRangeProcessor = true;
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(CompressorViewModelFactory);
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(CompressorSettingModelFactory);
            regView(CompressorEffect::Symbol, u"qrc:/dynamics/compressor/CompressorView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Compressor"),
                    muse::mtrc("effects", "Reduces “dynamic range”, or differences between loud and quiet parts"),
                    true
                    );
        } else if (symbol == LimiterEffect::Symbol) {
            hasDynamicRangeProcessor = true;
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(LimiterViewModelFactory);
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(LimiterSettingModelFactory);
            regView(LimiterEffect::Symbol, u"qrc:/dynamics/limiter/LimiterView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Limiter"),
                    muse::mtrc("effects", "Augments loudness while minimizing distortion"),
                    true
                    );
        } else if (symbol == NormalizeEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NormalizeViewModelFactory);
            regView(NormalizeEffect::Symbol, u"qrc:/normalize/NormalizeView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Normalize"),
                    muse::mtrc("effects", "Sets the peak amplitude of a one or more tracks"),
                    false
                    );
        } else if (symbol == RemoveDCOffsetEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Remove DC offset"),
                    muse::mtrc("effects", "Removes DC offset from the audio"),
                    true
                    );
        } else if (symbol == FadeInEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Fade in"),
                    muse::mtrc("effects", "Applies a linear fade-in to the selected audio"),
                    true
                    );
        } else if (symbol == FadeOutEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Fade out"),
                    muse::mtrc("effects", "Applies a linear fade-out to the selected audio"),
                    true
                    );
        } else if (symbol == InvertEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Invert"),
                    muse::mtrc("effects", "Flips the audio samples upside-down, reversing their polarity"),
                    true
                    );
        } else if (symbol == Repair::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Repair"),
                    muse::mtrc("effects", "Sets the peak amplitude of a one or more tracks"),
                    false
                    );
        } else if (symbol == ReverseEffect::Symbol) {
            regMeta(desc,
                    muse::mtrc("effects", "Reverse"),
                    muse::mtrc("effects", "Reverses the selected audio"),
                    true
                    );
        } else if (symbol == TruncateSilenceEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(TruncateSilenceViewModelFactory);
            regView(TruncateSilenceEffect::Symbol, u"qrc:/truncatesilence/TruncateSilenceView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Truncate silence"),
                    muse::mtrc("effects", "Automatically reduces the length of passages where the volume is below a specified level"),
                    true
                    );
        }
#if USE_SOUNDTOUCH
        else if (symbol == ChangePitchEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ChangePitchViewModelFactory);
            regView(ChangePitchEffect::Symbol, u"qrc:/changepitch/ChangePitchView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Change pitch"),
                    muse::mtrc("effects", "Changes the pitch of a track without changing its tempo"),
                    true
                    );
        }
#endif
        else if (symbol == ChirpEffect::Symbol) {
            regView(ChirpEffect::Symbol, u"qrc:/tonegen/ChirpView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Chirp"),
                    muse::mtrc("effects", "Generates an ascending or descending tone of one of four types"),
                    false
                    );
        } else if (symbol == ToneEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ToneViewModelFactory);
            regView(ToneEffect::Symbol, u"qrc:/tonegen/ToneView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Tone"),
                    muse::mtrc("effects", "Generates a constant frequency tone of one of four types"),
                    false
                    );
        } else if (symbol == ReverbEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ReverbViewModelFactory);
            regView(ReverbEffect::Symbol, u"qrc:/reverb/ReverbView.qml");
            regMeta(desc,
                    muse::mtrc("effects", "Reverb"),
                    muse::mtrc("effects", "Reverb effect"),
                    true
                    );
        } else if (symbol == NoiseGenerator::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NoiseViewModelFactory);
            regView(NoiseGenerator::Symbol, u"qrc:/noisegen/NoiseView.qml");
            regMeta(desc,
                    muse::mtrc("effects/noise", "Noise"),
                    muse::mtrc("effects/noise", "Generates noise"),
                    false
                    );
        } else if (symbol == NoiseReductionEffect::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NoiseReductionViewModelFactory);
            regView(NoiseReductionEffect::Symbol, u"qrc:/noisereduction/NoiseReductionView.qml");
            regMeta(desc,
                    muse::mtrc("effects/noisereduction", "Noise reduction"),
                    muse::mtrc("effects/noisereduction", "Reduces noise in the audio"),
                    false
                    );
        } else if (symbol == DtmfGenerator::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(DtmfViewModelFactory);
            regView(DtmfGenerator::Symbol, u"qrc:/dtmfgen/DtmfView.qml");
            regMeta(desc,
                    muse::mtrc("effects/dtmf", "DTMF tones"),
                    muse::mtrc("effects/dtmf", "Generates DTMF signal"),
                    false
                    );
        } else if (symbol == SilenceGenerator::Symbol) {
            REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(SilenceViewModelFactory);
            regView(SilenceGenerator::Symbol, u"qrc:/silencegen/SilenceView.qml");
            regMeta(desc,
                    muse::mtrc("effects/silence", "Silence"),
                    muse::mtrc("effects/silence", "Generates silence"),
                    false
                    );
        } else {
            LOGW() << "effect not found for symbol: " << au3::wxToStdString(symbol.Internal());
        }
    }

    if (hasDynamicRangeProcessor) {
        // These types are used by both Compressor and Limiter, so register them only if at least one of these effects is present.
        qmlRegisterType<DynamicsTimeline>("Audacity.BuiltinEffectsCollection", 1, 0, "DynamicsTimeline");
        qmlRegisterType<TimelineSourceModel>("Audacity.BuiltinEffectsCollection", 1, 0, "TimelineSourceModel");
        qmlRegisterType<CompressionDbMeterModel>("Audacity.BuiltinEffectsCollection", 1, 0, "CompressionDbMeterModel");
        qmlRegisterType<OutputDbMeterModel>("Audacity.BuiltinEffectsCollection", 1, 0, "OutputDbMeterModel");
        qmlRegisterType<Stopwatch>("Audacity.BuiltinEffectsCollection", 1, 0, "Stopwatch");
        qmlRegisterType<DynamicsPlayStateModel>("Audacity.BuiltinEffectsCollection", 1, 0, "DynamicsPlayStateModel");
    }
}
