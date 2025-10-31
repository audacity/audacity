/*
 * Audacity: A Digital Audio Editor
 */
#include "limitersettingmodel.h"

#include "global/log.h"
#include "global/translation.h"

#include <unordered_map>

namespace au::effects {
namespace {
const LimiterSettingModel::LabelMap labelMap = {
    { "thresholdDb", { muse::qtrc("effects", "Threshold"), muse::qtrc("effects", "dB") } },
    { "makeupTargetDb", { muse::qtrc("effects", "Output"), muse::qtrc("effects", "dB") } },
    { "kneeWidthDb", { muse::qtrc("effects", "Knee width"), muse::qtrc("effects", "dB") } },
    { "lookaheadMs", { muse::qtrc("effects", "Lookahead"), muse::qtrc("effects", "ms") } },
    { "releaseMs", { muse::qtrc("effects", "Release"), muse::qtrc("effects", "ms") } },
    // We deliberately leave out the "show" parameters: these are overridden in the UI anyway.
};
}

LimiterSettingModel::LimiterSettingModel(QObject* parent, int instanceId)
    : EffectSettingModelImpl<LimiterEffect>(parent, instanceId, labelMap, [this](const LimiterEffect& effect) {
    if (m_paramId == "thresholdDb") {
        return effect.thresholdDb;
    } else if (m_paramId == "makeupTargetDb") {
        return effect.makeupTargetDb;
    } else if (m_paramId == "kneeWidthDb") {
        return effect.kneeWidthDb;
    } else if (m_paramId == "lookaheadMs") {
        return effect.lookaheadMs;
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
