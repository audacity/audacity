/*
 * Audacity: A Digital Audio Editor
 */
#include "compressorviewmodel.h"

#include "libraries/lib-dynamic-range-processor/CompressorProcessor.h"

#include "log.h"

namespace au::effects {
CompressorViewModel::CompressorViewModel(QObject* parent)
    : AbstractEffectModel{parent}
{
}

QList<float> CompressorViewModel::compressionCurve(const QList<float>& dbIn) const
{
    const auto& s = settings<CompressorSettings>();
    QList<float> dbOut;
    dbOut.reserve(dbIn.size());
    for (const auto db : dbIn) {
        dbOut.append(CompressorProcessor::EvaluateTransferFunction(s, db));
    }
    return dbOut;
}

void CompressorViewModel::doReload()
{
    emit compressionCurveChanged();
}

CompressorSettingModel::CompressorSettingModel(QObject* parent)
    : EffectSettingModelImpl<CompressorEffect>(parent, [this](const CompressorEffect& effect) {
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
