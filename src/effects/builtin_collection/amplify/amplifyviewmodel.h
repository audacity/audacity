/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/builtin/view/builtineffectmodel.h"

namespace au::effects {
class AmplifyEffect;
class AmplifyViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString effectTitle READ effectTitle CONSTANT FINAL)

    Q_PROPERTY(QString ampLabel READ ampLabel CONSTANT FINAL)
    Q_PROPERTY(float ampValue READ ampValue WRITE setAmpValue NOTIFY isApplyAllowedChanged FINAL)
    Q_PROPERTY(float ampMin READ ampMin NOTIFY isApplyAllowedChanged FINAL)
    Q_PROPERTY(float ampMax READ ampMax NOTIFY isApplyAllowedChanged FINAL)
    Q_PROPERTY(QString ampMeasureUnitsSymbol READ ampMeasureUnitsSymbol CONSTANT FINAL)
    Q_PROPERTY(int ampDecimals READ ampDecimals CONSTANT FINAL)
    Q_PROPERTY(double ampStep READ ampStep CONSTANT FINAL)

    Q_PROPERTY(QString newPeakLabel READ newPeakLabel CONSTANT FINAL)
    Q_PROPERTY(float newPeakValue READ newPeakValue WRITE setNewPeakValue NOTIFY isApplyAllowedChanged FINAL)
    Q_PROPERTY(float newPeakMin READ newPeakMin NOTIFY isApplyAllowedChanged FINAL)
    Q_PROPERTY(float newPeakMax READ newPeakMax NOTIFY isApplyAllowedChanged FINAL)
    Q_PROPERTY(QString newPeakMeasureUnitsSymbol READ newPeakMeasureUnitsSymbol CONSTANT FINAL)
    Q_PROPERTY(int newPeakDecimals READ newPeakDecimals CONSTANT FINAL)
    Q_PROPERTY(double newPeakStep READ newPeakStep CONSTANT FINAL)

    Q_PROPERTY(QString canClipLabel READ canClipLabel CONSTANT FINAL)
    Q_PROPERTY(bool canClip READ canClip WRITE setCanClip NOTIFY canClipChanged FINAL)

    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

public:
    AmplifyViewModel(QObject* parent, int instanceId);
    ~AmplifyViewModel() override = default;

    QString effectTitle() const;

    QString ampLabel() const;
    float ampValue() const;
    void setAmpValue(float newAmpValue);
    float ampMin() const;
    float ampMax() const;
    QString ampMeasureUnitsSymbol() const;
    int ampDecimals() const;
    double ampStep() const;

    QString newPeakLabel() const;
    float newPeakValue() const;
    void setNewPeakValue(float newNewPeakValue);
    float newPeakMin() const;
    float newPeakMax() const;
    QString newPeakMeasureUnitsSymbol() const;
    int newPeakDecimals() const;
    double newPeakStep() const;

    QString canClipLabel() const;
    bool canClip() const;
    void setCanClip(bool newClipping);

    bool isApplyAllowed() const;

signals:
    void canClipChanged();
    void isApplyAllowedChanged();

private:
    void doReload() override;
};

class AmplifyViewModelFactory : public EffectViewModelFactory<AmplifyViewModel>
{
};
}
