/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/builtineffectmodel.h"

namespace au::effects {
class GeneralViewModel : public BuiltinEffectModel
{
    Q_OBJECT
public:
    GeneralViewModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

private:

    Effect* m_effect = nullptr;
};
}
