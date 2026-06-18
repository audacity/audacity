/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/view/builtineffectmodel.h"

namespace au::effects {
class DeepFilterNetViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString effectTitle READ effectTitle CONSTANT FINAL)
    Q_PROPERTY(double attenuationLimit READ attenuationLimit WRITE setAttenuationLimit NOTIFY attenuationLimitChanged FINAL)
    Q_PROPERTY(double attenuationLimitMin READ attenuationLimitMin CONSTANT FINAL)
    Q_PROPERTY(double attenuationLimitMax READ attenuationLimitMax CONSTANT FINAL)
    Q_PROPERTY(double mix READ mix WRITE setMix NOTIFY mixChanged FINAL)
    Q_PROPERTY(double mixMin READ mixMin CONSTANT FINAL)
    Q_PROPERTY(double mixMax READ mixMax CONSTANT FINAL)

public:
    DeepFilterNetViewModel(QObject* parent, int instanceId);

    QString effectTitle() const;

    double attenuationLimit() const;
    void setAttenuationLimit(double value);
    double attenuationLimitMin() const;
    double attenuationLimitMax() const;

    double mix() const;
    void setMix(double value);
    double mixMin() const;
    double mixMax() const;

signals:
    void attenuationLimitChanged();
    void mixChanged();

private:
    void doReload() override;
};

class DeepFilterNetViewModelFactory : public EffectViewModelFactory<DeepFilterNetViewModel>
{
};
}
