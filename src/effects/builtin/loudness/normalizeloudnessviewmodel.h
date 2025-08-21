/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/builtineffectmodel.h"

namespace au::effects {
class NormalizeLoudnessEffect;
class NormalizeLoudnessViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString effectTitle READ effectTitle CONSTANT FINAL)
    Q_PROPERTY(QStringList algorithmOptions READ algorithmOptions CONSTANT FINAL)
    Q_PROPERTY(bool useRmsAlgorithm READ useRmsAlgorithm WRITE setUseRmsAlgorithm NOTIFY useRmsAlgorithmChanged FINAL)
    Q_PROPERTY(QString toLabel READ toLabel CONSTANT FINAL)
    Q_PROPERTY(QString currentMeasureUnitsSymbol READ currentMeasureUnitsSymbol NOTIFY useRmsAlgorithmChanged FINAL)
    Q_PROPERTY(
        double perceivedLoudnessTarget READ perceivedLoudnessTarget WRITE setPerceivedLoudnessTarget NOTIFY perceivedLoudnessTargetChanged FINAL)
    Q_PROPERTY(double rmsTarget READ rmsTarget WRITE setRmsTarget NOTIFY rmsTargetChanged FINAL)
    Q_PROPERTY(double targetMin READ targetMin CONSTANT FINAL)
    Q_PROPERTY(double targetMax READ targetMax CONSTANT FINAL)
    Q_PROPERTY(int targetStep READ targetStep CONSTANT FINAL)
    Q_PROPERTY(int targetDecimals READ targetDecimals CONSTANT FINAL)
    Q_PROPERTY(QString normalizeLabel READ normalizeLabel CONSTANT FINAL)
    Q_PROPERTY(
        bool normalizeStereoChannelsIndependently READ normalizeStereoChannelsIndependently WRITE setNormalizeStereoChannelsIndependently NOTIFY normalizeStereoChannelsIndependentlyChanged FINAL)
    Q_PROPERTY(QString independentStereoLabel READ independentStereoLabel CONSTANT FINAL)
    Q_PROPERTY(QString useDualMonoLabel READ useDualMonoLabel CONSTANT FINAL)
    Q_PROPERTY(bool useDualMono READ useDualMono WRITE setUseDualMono NOTIFY useDualMonoChanged FINAL)

public:
    NormalizeLoudnessViewModel() = default;

    QString effectTitle() const;

    QStringList algorithmOptions() const;
    bool useRmsAlgorithm() const;
    void setUseRmsAlgorithm(bool useRmsAlgorithm);

    QString toLabel() const;
    QString currentMeasureUnitsSymbol() const;

    double perceivedLoudnessTarget() const;
    void setPerceivedLoudnessTarget(double perceivedLoudnessTarget);

    double rmsTarget() const;
    void setRmsTarget(double rmsTarget);

    double targetMin() const;
    double targetMax() const;
    int targetStep() const;
    int targetDecimals() const;

    QString normalizeLabel() const;
    bool normalizeStereoChannelsIndependently() const;
    void setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently);

    QString independentStereoLabel() const;
    QString useDualMonoLabel() const;
    bool useDualMono() const;
    void setUseDualMono(bool newUseDualMono);

signals:
    void useRmsAlgorithmChanged();
    void perceivedLoudnessTargetChanged();
    void rmsTargetChanged();
    void normalizeStereoChannelsIndependentlyChanged();
    void useDualMonoChanged();

private:
    void doReload() override;
};
}
