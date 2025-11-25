/*
 * Audacity: A Digital Audio Editor
 */
#include "compressorviewmodel.h"

#include "libraries/lib-dynamic-range-processor/CompressorProcessor.h"

#include "log.h"

namespace au::effects {
CompressorViewModel::CompressorViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel{parent, instanceId}
{
}

QList<QVariantMap> CompressorViewModel::compressionCurve(int from, int to, int count) const
{
    const auto& s = settings<CompressorSettings>();
    QList<QVariantMap> points;
    points.reserve(count);
    for (int i = 0; i < count; ++i) {
        const float db = from + (to - from) * i / (count - 1);
        points.append({ { "x", db }, { "y", CompressorProcessor::EvaluateTransferFunction(s, db) } });
    }
    return points;
}

void CompressorViewModel::doReload()
{
    emit compressionCurveChanged();
}
}
