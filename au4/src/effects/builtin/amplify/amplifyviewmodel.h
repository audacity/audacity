/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../abstracteffectmodel.h"

namespace au::effects {
class AmplifyEffect;
class AmplifyViewModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(double ratio READ ratio WRITE setRatio NOTIFY ratioChanged FINAL)

public:
    AmplifyViewModel() = default;

    Q_INVOKABLE void init();

    double ratio() const;
    void setRatio(double newRatio);

signals:
    void ratioChanged();

private:
    AmplifyEffect* m_effect = nullptr;
};
}
