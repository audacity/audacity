/*
 * Audacity: A Digital Audio Editor
 */
#include "compressorsettingmodel.h"

#include "global/translation.h"

#include <unordered_map>

namespace au::effects {
namespace {
const CompressorSettingModel::LabelMap labelMap = {
    { "thresholdDb", { muse::qtrc("effects", "Threshold"), muse::qtrc("effects", "dB") } },
    { "makeupGainDb", { muse::qtrc("effects", "Make-up gain"), muse::qtrc("effects", "dB") } },
    { "kneeWidthDb", { muse::qtrc("effects", "Knee width"), muse::qtrc("effects", "dB") } },
    { "compressionRatio", { muse::qtrc("effects", "Ratio"), QString() } },
    { "lookaheadMs", { muse::qtrc("effects", "Lookahead"), muse::qtrc("effects", "ms") } },
    { "attackMs", { muse::qtrc("effects", "Attack"), muse::qtrc("effects", "ms") } },
    { "releaseMs", { muse::qtrc("effects", "Release"), muse::qtrc("effects", "ms") } },
    // We deliberately leave out the "show" parameters: these are overridden in the UI anyway.
};
}

CompressorSettingModel::CompressorSettingModel(QObject* parent, int instanceId)
    : EffectSettingModelImpl<CompressorEffect>(parent, instanceId, labelMap, [this](const CompressorEffect& effect) {
    if (m_paramId == "thresholdDb") {
        return effect.thresholdDb;
    } else if (m_paramId == "makeupGainDb") {
        return effect.makeupGainDb;
    } else if (m_paramId == "kneeWidthDb") {
        return effect.kneeWidthDb;
    } else if (m_paramId == "compressionRatio") {
        return effect.compressionRatio;
    } else if (m_paramId == "lookaheadMs") {
        return effect.lookaheadMs;
    } else if (m_paramId == "attackMs") {
        return effect.attackMs;
    } else if (m_paramId == "releaseMs") {
        return effect.releaseMs;
    } else if (m_paramId == "showInput") {
        return effect.showInput;
    } else if (m_paramId == "showOutput") {
        return effect.showOutput;
    } else if (m_paramId == "showActual") {
        return effect.showActual;
    } else if (m_paramId == "showTarget") {
        return effect.showTarget;
    } else {
        LOGE("Unknown parameter ID: ") << m_paramId;
        assert(false);
        return effect.thresholdDb;
    }
}) {}
}
