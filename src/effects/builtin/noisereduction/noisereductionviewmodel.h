/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

#include "effects/effects_base/ieffectsprovider.h"
#include "effects/effects_base/ieffectinstancesregister.h"

namespace au::effects {
class NoiseReductionEffect;

enum class NoiseReductionMode {
    ReduceNoise = 0,
    LeaveResidue
};

class NoiseReductionViewModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)
    Q_PROPERTY(int reduction READ reduction WRITE setReduction NOTIFY reductionChanged FINAL)
    Q_PROPERTY(float sensitivity READ sensitivity WRITE setSensitivity NOTIFY sensitivityChanged FINAL)
    Q_PROPERTY(
        int frequencySmoothingBands READ frequencySmoothingBands WRITE setFrequencySmoothingBands NOTIFY frequencySmoothingBandsChanged FINAL)
    Q_PROPERTY(NoiseReductionMode reductionMode READ reductionMode WRITE setReductionMode NOTIFY reductionModeChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:
    NoiseReductionViewModel() = default;

    bool isApplyAllowed() const;
    void setIsApplyAllowed(bool isApplyAllowed);

    int reduction() const;
    void setReduction(int reduction);

    float sensitivity() const;
    void setSensitivity(float sensitivity);

    int frequencySmoothingBands() const;
    void setFrequencySmoothingBands(int frequencySmoothingBands);

    NoiseReductionMode reductionMode() const;
    void setReductionMode(NoiseReductionMode mode);

    Q_INVOKABLE bool getNoiseProfile();

signals:
    void isApplyAllowedChanged();
    void reductionChanged();
    void sensitivityChanged();
    void frequencySmoothingBandsChanged();
    void reductionModeChanged();

private:
    void doReload() override;

    NoiseReductionEffect* effect() const;
};
}
