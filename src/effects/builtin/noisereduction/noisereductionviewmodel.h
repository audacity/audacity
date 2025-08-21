/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/builtineffectmodel.h"
#include "../common/params.h"

#include "global/iinteractive.h"

namespace au::effects {
class NoiseReductionEffect;

class NoiseReductionViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

    Q_PROPERTY(int reduction READ reduction WRITE setReduction NOTIFY reductionChanged FINAL)
    Q_PROPERTY(int reductionMin READ reductionMin CONSTANT FINAL)
    Q_PROPERTY(int reductionMax READ reductionMax CONSTANT FINAL)

    Q_PROPERTY(double sensitivity READ sensitivity WRITE setSensitivity NOTIFY sensitivityChanged FINAL)
    Q_PROPERTY(double sensitivityMin READ sensitivityMin CONSTANT FINAL)
    Q_PROPERTY(double sensitivityMax READ sensitivityMax CONSTANT FINAL)

    Q_PROPERTY(
        int frequencySmoothingBands READ frequencySmoothingBands WRITE setFrequencySmoothingBands NOTIFY frequencySmoothingBandsChanged FINAL)
    Q_PROPERTY(int frequencySmoothingBandsMin READ frequencySmoothingBandsMin CONSTANT FINAL)
    Q_PROPERTY(int frequencySmoothingBandsMax READ frequencySmoothingBandsMax CONSTANT FINAL)

    Q_PROPERTY(int reductionMode READ reductionMode WRITE setReductionMode NOTIFY reductionModeChanged FINAL)

    muse::Inject<muse::IInteractive> interactive;

public:
    NoiseReductionViewModel() = default;

    bool isApplyAllowed() const;
    void setIsApplyAllowed(bool isApplyAllowed);

    int reduction() const;
    void setReduction(int reduction);
    int reductionMin() const;
    int reductionMax() const;

    double sensitivity() const;
    void setSensitivity(double sensitivity);
    double sensitivityMin() const;
    double sensitivityMax() const;

    int frequencySmoothingBands() const;
    void setFrequencySmoothingBands(int frequencySmoothingBands);
    int frequencySmoothingBandsMin();
    int frequencySmoothingBandsMax() const;

    int reductionMode() const;
    void setReductionMode(int mode);

    Q_INVOKABLE void getNoiseProfile();

signals:
    void isApplyAllowedChanged();
    void reductionChanged();
    void sensitivityChanged();
    void frequencySmoothingBandsChanged();
    void reductionModeChanged();

private:
    void doReload() override;
    bool usesPresets() const override { return false; }
};
}
