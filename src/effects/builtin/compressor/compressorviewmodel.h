/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "compressoreffect.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class CompressorViewModel : public AbstractEffectModel
{
    Q_OBJECT

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    CompressorViewModel(QObject* parent = nullptr);

    Q_INVOKABLE QList<float> compressionCurve(const QList<float>& dbIn) const;

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
