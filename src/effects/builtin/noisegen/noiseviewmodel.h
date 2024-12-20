/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/generatoreffectmodel.h"

namespace au::effects {
class NoiseGenerator;

class NoiseViewModel : public GeneratorEffectModel
{
    Q_OBJECT

    Q_PROPERTY(double amplitude READ amplitude WRITE prop_setAmplitude NOTIFY amplitudeChanged)
    Q_PROPERTY(int type READ type WRITE prop_setType NOTIFY typeChanged)
    Q_PROPERTY(QVariantList types READ types CONSTANT)

public:
    bool isApplyAllowed() const override;

    QVariantList types() const;

    double amplitude() const;
    void prop_setAmplitude(double newAmplitude);

    int type() const;
    void prop_setType(int type);

signals:
    void amplitudeChanged();
    void typeChanged();

private:

    void doEmitSignals() override;
};
}
