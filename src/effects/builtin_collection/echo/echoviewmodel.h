#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
class EchoViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(double delay READ delay WRITE setDelay NOTIFY delayChanged FINAL)
    Q_PROPERTY(double decay READ decay WRITE setDecay NOTIFY decayChanged FINAL)

    Q_PROPERTY(QString delayLabel READ delayLabel CONSTANT FINAL)
    Q_PROPERTY(double delayMin READ delayMin CONSTANT FINAL)
    Q_PROPERTY(double delayMax READ delayMax CONSTANT FINAL)
    Q_PROPERTY(double delayStep READ delayStep CONSTANT FINAL)
    Q_PROPERTY(int delayDecimals READ delayDecimals CONSTANT FINAL)
    Q_PROPERTY(QString delayUnitSymbol READ delayUnitSymbol CONSTANT FINAL)

    Q_PROPERTY(QString decayLabel READ decayLabel CONSTANT FINAL)
    Q_PROPERTY(double decayMin READ decayMin CONSTANT FINAL)
    Q_PROPERTY(double decayMax READ decayMax CONSTANT FINAL)
    Q_PROPERTY(double decayStep READ decayStep CONSTANT FINAL)
    Q_PROPERTY(int decayDecimals READ decayDecimals CONSTANT FINAL)

public:
    EchoViewModel(QObject* parent, int instanceId);
    ~EchoViewModel() override = default;

    double delay() const;
    void setDelay(double newDelay);

    double decay() const;
    void setDecay(double newDecay);

    QString delayLabel() const;
    double delayMin() const;
    double delayMax() const;
    double delayStep() const;
    int delayDecimals() const;
    QString delayUnitSymbol() const;

    QString decayLabel() const;
    double decayMin() const;
    double decayMax() const;
    double decayStep() const;
    int decayDecimals() const;

signals:
    void delayChanged();
    void decayChanged();

private:
    void doReload() override;
};

class EchoViewModelFactory : public EffectViewModelFactory<EchoViewModel>
{
};
}
