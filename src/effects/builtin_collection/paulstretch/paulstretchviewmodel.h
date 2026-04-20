/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/builtin/view/builtineffectmodel.h"

namespace au::effects {
class PaulstretchEffect;
class PaulstretchViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(double amount READ amount WRITE setAmount NOTIFY amountChanged FINAL)
    Q_PROPERTY(double timeResolution READ timeResolution WRITE setTimeResolution NOTIFY timeResolutionChanged FINAL)

    Q_PROPERTY(QString stretchFactorLabel READ stretchFactorLabel CONSTANT FINAL)
    Q_PROPERTY(double stretchFactorMin READ stretchFactorMin CONSTANT FINAL)
    Q_PROPERTY(double stretchFactorMax READ stretchFactorMax CONSTANT FINAL)
    Q_PROPERTY(double stretchFactorStep READ stretchFactorStep CONSTANT FINAL)
    Q_PROPERTY(int stretchFactorDecimals READ stretchFactorDecimals CONSTANT FINAL)

    Q_PROPERTY(QString timeResolutionLabel READ timeResolutionLabel CONSTANT FINAL)
    Q_PROPERTY(double timeResolutionMin READ timeResolutionMin CONSTANT FINAL)
    Q_PROPERTY(double timeResolutionMax READ timeResolutionMax CONSTANT FINAL)
    Q_PROPERTY(double timeResolutionStep READ timeResolutionStep CONSTANT FINAL)
    Q_PROPERTY(int timeResolutionDecimals READ timeResolutionDecimals CONSTANT FINAL)
    Q_PROPERTY(QString timeResolutionUnitSymbol READ timeResolutionUnitSymbol CONSTANT FINAL)

public:
    PaulstretchViewModel(QObject* parent, int instanceId);
    ~PaulstretchViewModel() override = default;

    double amount() const;
    void setAmount(double newAmount);

    double timeResolution() const;
    void setTimeResolution(double newTimeResolution);

    QString stretchFactorLabel() const;
    double stretchFactorMin() const;
    double stretchFactorMax() const;
    double stretchFactorStep() const;
    int stretchFactorDecimals() const;

    QString timeResolutionLabel() const;
    double timeResolutionMin() const;
    double timeResolutionMax() const;
    double timeResolutionStep() const;
    int timeResolutionDecimals() const;
    QString timeResolutionUnitSymbol() const;

signals:
    void amountChanged();
    void timeResolutionChanged();

private:
    void doReload() override;
};

class PaulstretchViewModelFactory : public EffectViewModelFactory<PaulstretchViewModel>
{
};
}
