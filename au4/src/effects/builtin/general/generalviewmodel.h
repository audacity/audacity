/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../abstracteffectmodel.h"

class Effect;
namespace au::effects {
class GeneralViewModel : public AbstractEffectModel
{
    Q_OBJECT
public:
    GeneralViewModel() = default;

    Q_INVOKABLE void init();

private:

    Effect* m_effect = nullptr;
};
}
