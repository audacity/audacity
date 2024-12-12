/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/generatoreffectmodel.h"
#include "noisegenerator.h"

namespace au::effects {
class NoiseGenerator;

class NoiseViewModel : public GeneratorEffectModel
{
    Q_OBJECT

    Q_PROPERTY(double amplitude READ amplitude WRITE setAmplitude NOTIFY amplitudeChanged)
    Q_PROPERTY(int type READ type WRITE setType NOTIFY typeChanged)
    Q_PROPERTY(QVariantList types READ types CONSTANT)

public:
    QVariantList types() const;
    double amplitude() const;
    void setAmplitude(double newAmplitude);
    int type() const;
    void setType(int type);

signals:
    void amplitudeChanged();
    void typeChanged();

private:

    void doEmitSignals() override;
};
}
