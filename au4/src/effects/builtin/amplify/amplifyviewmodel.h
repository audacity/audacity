/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

namespace au::effects {
class AmplifyEffect;
class AmplifyViewModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(double amp READ amp WRITE setAmp NOTIFY ampChanged FINAL)
    Q_PROPERTY(double ampMin READ ampMin NOTIFY ampChanged FINAL)
    Q_PROPERTY(double ampMax READ ampMax NOTIFY ampChanged FINAL)

    Q_PROPERTY(double newPeak READ newPeak WRITE setNewPeak NOTIFY newPeakChanged FINAL)
    Q_PROPERTY(double newPeakMin READ newPeakMin NOTIFY newPeakChanged FINAL)
    Q_PROPERTY(double newPeakMax READ newPeakMax NOTIFY newPeakChanged FINAL)

    Q_PROPERTY(bool canClip READ canClip WRITE setCanClip NOTIFY canClipChanged FINAL)

    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

public:
    AmplifyViewModel() = default;

    Q_INVOKABLE void init();

    double amp() const;
    void setAmp(double newAmp);
    double ampMin() const;
    double ampMax() const;

    double newPeak() const;
    void setNewPeak(double newNewPeak);
    double newPeakMin() const;
    double newPeakMax() const;

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
    AmplifyEffect* effect() const;

    void update();

    Param<double> m_amp;
    double m_newPeak = 0.0;
    bool m_canClip = false;
    bool m_isApplyAllowed = false;
};
}
