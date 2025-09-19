/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/builtin/common/builtineffectmodel.h"

namespace au::effects {
class CompressorViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    CompressorViewModel(QObject* parent = nullptr);

    Q_INVOKABLE QList<QVariantMap> compressionCurve(int from, int to, int count) const;

signals:
    void compressionCurveChanged();

private:
    void doReload() override;
};
}
