/*
 * Audacity: A Digital Audio Editor
 */
#include "limiterviewmodel.h"

#include "log.h"

namespace au::effects {
LimiterViewModel::LimiterViewModel(QObject* parent)
    : BuiltinEffectModel{parent}
{
}

void LimiterViewModel::doReload()
{
}

LimiterSettingModel::LimiterSettingModel(QObject* parent)
    : EffectSettingModelImpl<LimiterEffect>(parent, [this](const LimiterEffect& effect) {
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
