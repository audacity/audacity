/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class AmplifyEffect;
class AmplifyViewModel : public AbstractEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title CONSTANT FINAL)

    Q_PROPERTY(QString ampLabel READ ampLabel CONSTANT FINAL)
    Q_PROPERTY(float amp READ amp WRITE setAmp NOTIFY ampChanged FINAL)
    Q_PROPERTY(float ampMin READ ampMin NOTIFY ampChanged FINAL)
    Q_PROPERTY(float ampMax READ ampMax NOTIFY ampChanged FINAL)
    Q_PROPERTY(QString ampMeasureUnitsSymbol READ ampMeasureUnitsSymbol CONSTANT FINAL)
    Q_PROPERTY(int ampDecimals READ ampDecimals CONSTANT FINAL)
    Q_PROPERTY(double ampStep READ ampStep CONSTANT FINAL)

    Q_PROPERTY(QString newPeakLabel READ newPeakLabel CONSTANT FINAL)
    Q_PROPERTY(float newPeak READ newPeak WRITE setNewPeak NOTIFY newPeakChanged FINAL)
    Q_PROPERTY(float newPeakMin READ newPeakMin NOTIFY newPeakChanged FINAL)
    Q_PROPERTY(float newPeakMax READ newPeakMax NOTIFY newPeakChanged FINAL)
    Q_PROPERTY(QString newPeakMeasureUnitsSymbol READ newPeakMeasureUnitsSymbol CONSTANT FINAL)
    Q_PROPERTY(int newPeakDecimals READ newPeakDecimals CONSTANT FINAL)
    Q_PROPERTY(double newPeakStep READ newPeakStep CONSTANT FINAL)

    Q_PROPERTY(QString canClipLabel READ canClipLabel CONSTANT FINAL)
    Q_PROPERTY(bool canClip READ canClip WRITE setCanClip NOTIFY canClipChanged FINAL)

    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    AmplifyViewModel() = default;

    QString title() const;

    QString ampLabel() const;
    float amp() const;
    void setAmp(float newAmp);
    float ampMin() const;
    float ampMax() const;
    QString ampMeasureUnitsSymbol() const;
    int ampDecimals() const;
    double ampStep() const;

    QString newPeakLabel() const;
    float newPeak() const;
    void setNewPeak(float newNewPeak);
    float newPeakMin() const;
    float newPeakMax() const;
    QString newPeakMeasureUnitsSymbol() const;
    int newPeakDecimals() const;
    double newPeakStep() const;

    QString canClipLabel() const;
    bool canClip() const;
    void setCanClip(bool newCliping);

    bool isApplyAllowed() const;
    void setIsApplyAllowed(bool isApplyAllowed);

signals:
    void ampChanged();
    void newPeakChanged();
    void canClipChanged();
    void isApplyAllowedChanged();

private:
    void doReload() override;

    AmplifyEffect* effect() const;

    void update();

    Param<db_t> m_amp;
    db_t m_newPeak = 0.0f;
    bool m_canClip = false;
    bool m_isApplyAllowed = false;
};
}
