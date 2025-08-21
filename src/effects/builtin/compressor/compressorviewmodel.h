/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "compressoreffect.h"

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

class CompressorSettingModel : public EffectSettingModelImpl<CompressorEffect>
{
public:
    CompressorSettingModel(QObject* parent = nullptr);
};
}
