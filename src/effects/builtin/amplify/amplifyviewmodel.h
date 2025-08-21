/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/builtineffectmodel.h"
#include "../common/params.h"

namespace au::effects {
class AmplifyEffect;
class AmplifyViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString effectTitle READ effectTitle CONSTANT FINAL)

    Q_PROPERTY(QString ampLabel READ ampLabel CONSTANT FINAL)
    Q_PROPERTY(float ampValue READ ampValue WRITE setAmpValue NOTIFY ampValueChanged FINAL)
    Q_PROPERTY(float ampMin READ ampMin NOTIFY ampValueChanged FINAL)
    Q_PROPERTY(float ampMax READ ampMax NOTIFY ampValueChanged FINAL)
    Q_PROPERTY(QString ampMeasureUnitsSymbol READ ampMeasureUnitsSymbol CONSTANT FINAL)
    Q_PROPERTY(int ampDecimals READ ampDecimals CONSTANT FINAL)
    Q_PROPERTY(double ampStep READ ampStep CONSTANT FINAL)

    Q_PROPERTY(QString newPeakLabel READ newPeakLabel CONSTANT FINAL)
    Q_PROPERTY(float newPeakValue READ newPeakValue WRITE setNewPeakValue NOTIFY newPeakValueChanged FINAL)
    Q_PROPERTY(float newPeakMin READ newPeakMin NOTIFY newPeakValueChanged FINAL)
    Q_PROPERTY(float newPeakMax READ newPeakMax NOTIFY newPeakValueChanged FINAL)
    Q_PROPERTY(QString newPeakMeasureUnitsSymbol READ newPeakMeasureUnitsSymbol CONSTANT FINAL)
    Q_PROPERTY(int newPeakDecimals READ newPeakDecimals CONSTANT FINAL)
    Q_PROPERTY(double newPeakStep READ newPeakStep CONSTANT FINAL)

    Q_PROPERTY(QString canClipLabel READ canClipLabel CONSTANT FINAL)
    Q_PROPERTY(bool canClip READ canClip WRITE setCanClip NOTIFY canClipChanged FINAL)

    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

public:
    AmplifyViewModel() = default;

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
    void setIsApplyAllowed(bool isApplyAllowed);

signals:
    void ampValueChanged();
    void newPeakValueChanged();
    void canClipChanged();
    void isApplyAllowedChanged();

private:
    void doReload() override;

    void update();

    Param<db_t> m_amp;
    db_t m_newPeak = 0.0f;
    bool m_canClip = false;
    bool m_isApplyAllowed = false;
};
}
