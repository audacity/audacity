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
    Q_PROPERTY(float amp READ amp WRITE setAmp NOTIFY ampChanged FINAL)
    Q_PROPERTY(float ampMin READ ampMin NOTIFY ampChanged FINAL)
    Q_PROPERTY(float ampMax READ ampMax NOTIFY ampChanged FINAL)

    Q_PROPERTY(float newPeak READ newPeak WRITE setNewPeak NOTIFY newPeakChanged FINAL)
    Q_PROPERTY(float newPeakMin READ newPeakMin NOTIFY newPeakChanged FINAL)
    Q_PROPERTY(float newPeakMax READ newPeakMax NOTIFY newPeakChanged FINAL)

    Q_PROPERTY(bool canClip READ canClip WRITE setCanClip NOTIFY canClipChanged FINAL)

    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    AmplifyViewModel() = default;

    float amp() const;
    void setAmp(float newAmp);
    float ampMin() const;
    float ampMax() const;

    float newPeak() const;
    void setNewPeak(float newNewPeak);
    float newPeakMin() const;
    float newPeakMax() const;

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
    db_t m_newPeak = 0.0;
    bool m_canClip = false;
    bool m_isApplyAllowed = false;
};
}
