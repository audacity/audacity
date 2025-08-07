/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class NormalizeLoudnessEffect;
class NormalizeLoudnessViewModel : public AbstractEffectModel
{
    Q_OBJECT

    muse::Inject<IEffectsProvider> effectsProvider;

    Q_PROPERTY(bool useRmsAlgorithm READ useRmsAlgorithm WRITE setUseRmsAlgorithm NOTIFY useRmsAlgorithmChanged FINAL)
    Q_PROPERTY(double perceivedLoudnessTarget READ perceivedLoudnessTarget WRITE setPerceivedLoudnessTarget NOTIFY perceivedLoudnessTargetChanged FINAL)
    Q_PROPERTY(double rmsTarget READ rmsTarget WRITE setRmsTarget NOTIFY rmsTargetChanged FINAL)
    Q_PROPERTY(double targetMin READ targetMin CONSTANT FINAL)
    Q_PROPERTY(double targetMax READ targetMax CONSTANT FINAL)
    Q_PROPERTY(bool normalizeStereoChannelsIndependently READ normalizeStereoChannelsIndependently WRITE setNormalizeStereoChannelsIndependently NOTIFY normalizeStereoChannelsIndependentlyChanged FINAL)
    Q_PROPERTY(bool dualMono READ dualMono WRITE setDualMono NOTIFY dualMonoChanged FINAL)

public:
    NormalizeLoudnessViewModel() = default;

    bool useRmsAlgorithm() const;
    void setUseRmsAlgorithm(bool useRmsAlgorithm);

    double perceivedLoudnessTarget() const;
    void setPerceivedLoudnessTarget(double perceivedLoudnessTarget);

    double rmsTarget() const;
    void setRmsTarget(double rmsTarget);

    double targetMin() const;
    double targetMax() const;

    bool normalizeStereoChannelsIndependently() const;
    void setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently);

    bool dualMono() const;
    void setDualMono(bool dualMono);

signals:
    void useRmsAlgorithmChanged();
    void perceivedLoudnessTargetChanged();
    void rmsTargetChanged();
    void normalizeStereoChannelsIndependentlyChanged();
    void dualMonoChanged();

private:
    void doReload() override;

    NormalizeLoudnessEffect* effect() const;
};
}
