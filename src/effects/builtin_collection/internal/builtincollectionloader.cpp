/*
 * Audacity: A Digital Audio Editor
 */
#include "builtincollectionloader.h"

#include <QtQml>

#include "global/translation.h"
#include "global/log.h"

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
#include "paulstretch/paulstretcheffect.h"
#include "paulstretch/paulstretchviewmodel.h"
#include "tonegen/toneviewmodel.h"
#include "dtmfgen/dtmfgenerator.h"
#include "dtmfgen/dtmfviewmodel.h"
#include "silencegen/silencegenerator.h"
#include "silencegen/silenceviewmodel.h"
#include "slidingstretch/slidingstretcheffect.h"
#include "slidingstretch/slidingstretchviewmodel.h"
#include "slidingstretch/slidingstretchsettingmodel.h"
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

using namespace au::effects;

void BuiltinCollectionLoader::preInit()
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
    static BuiltinEffectsModule::Registration< PaulstretchEffect > regPaulstretch;
    static BuiltinEffectsModule::Registration< SilenceGenerator > regSilence;
    static BuiltinEffectsModule::Registration< SlidingStretchEffect > regSlidingStretch;
    static BuiltinEffectsModule::Registration< NoiseGenerator > regNoise;
    static BuiltinEffectsModule::Registration< NoiseReductionEffect > regNoiseReduction;
    static BuiltinEffectsModule::Registration< DtmfGenerator > regDtmf;
    static BuiltinEffectsModule::Registration< CompressorEffect > regCompressor;
    static BuiltinEffectsModule::Registration< LimiterEffect > regLimiter;
}

void BuiltinCollectionLoader::init()
{
    auto regView = [this](const ::ComponentInterfaceSymbol& symbol, const muse::String& url) {
        builtinEffectsViewRegister()->regUrl(au3::wxToString(symbol.Internal()), url);
    };

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(AmplifyViewModelFactory);
    regView(AmplifyEffect::Symbol, u"qrc:/amplify/AmplifyView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NormalizeLoudnessViewModelFactory);
    regView(NormalizeLoudnessEffect::Symbol, u"qrc:/loudness/NormalizeLoudnessView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(GraphicEqViewModelFactory);
    qmlRegisterType<GraphicEqBandsModel>("Audacity.Effects", 1, 0, "GraphicEqBandsModel");
    regView(GraphicEq::Symbol, u"qrc:/graphiceq/GraphicEqView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ClickRemovalViewModelFactory);
    regView(ClickRemovalEffect::Symbol, u"qrc:/clickremoval/ClickRemovalView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(CompressorViewModelFactory);
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(CompressorSettingModelFactory);
    regView(CompressorEffect::Symbol, u"qrc:/dynamics/compressor/CompressorView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(LimiterViewModelFactory);
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(LimiterSettingModelFactory);
    regView(LimiterEffect::Symbol, u"qrc:/dynamics/limiter/LimiterView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NormalizeViewModelFactory);
    regView(NormalizeEffect::Symbol, u"qrc:/normalize/NormalizeView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(TruncateSilenceViewModelFactory);
    regView(TruncateSilenceEffect::Symbol, u"qrc:/truncatesilence/TruncateSilenceView.qml");

#if USE_SOUNDTOUCH
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ChangePitchViewModelFactory);
    regView(ChangePitchEffect::Symbol, u"qrc:/changepitch/ChangePitchView.qml");
#endif

    regView(ChirpEffect::Symbol, u"qrc:/tonegen/ChirpView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ToneViewModelFactory);
    regView(ToneEffect::Symbol, u"qrc:/tonegen/ToneView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(ReverbViewModelFactory);
    regView(ReverbEffect::Symbol, u"qrc:/reverb/ReverbView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(PaulstretchViewModelFactory);
    regView(PaulstretchEffect::Symbol, u"qrc:/paulstretch/PaulstretchView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NoiseViewModelFactory);
    regView(NoiseGenerator::Symbol, u"qrc:/noisegen/NoiseView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(NoiseReductionViewModelFactory);
    regView(NoiseReductionEffect::Symbol, u"qrc:/noisereduction/NoiseReductionView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(DtmfViewModelFactory);
    regView(DtmfGenerator::Symbol, u"qrc:/dtmfgen/DtmfView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(SilenceViewModelFactory);
    regView(SilenceGenerator::Symbol, u"qrc:/silencegen/SilenceView.qml");

    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(SlidingStretchViewModelFactory);
    REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(SlidingStretchSettingModelFactory);
    regView(SlidingStretchEffect::Symbol, u"qrc:/slidingstretch/SlidingStretchView.qml");

    qmlRegisterType<DynamicsTimeline>("Audacity.BuiltinEffectsCollection", 1, 0, "DynamicsTimeline");
    qmlRegisterType<TimelineSourceModel>("Audacity.BuiltinEffectsCollection", 1, 0, "TimelineSourceModel");
    qmlRegisterType<CompressionDbMeterModel>("Audacity.BuiltinEffectsCollection", 1, 0, "CompressionDbMeterModel");
    qmlRegisterType<OutputDbMeterModel>("Audacity.BuiltinEffectsCollection", 1, 0, "OutputDbMeterModel");
    qmlRegisterType<Stopwatch>("Audacity.BuiltinEffectsCollection", 1, 0, "Stopwatch");
    qmlRegisterType<DynamicsPlayStateModel>("Audacity.BuiltinEffectsCollection", 1, 0, "DynamicsPlayStateModel");
}
