/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../abstracteffectmodel.h"

namespace au::effects {
class AmplifyViewModel : public AbstractEffectModel
{
    Q_OBJECT
public:
    AmplifyViewModel() = default;

    Q_INVOKABLE void init();
};
}
