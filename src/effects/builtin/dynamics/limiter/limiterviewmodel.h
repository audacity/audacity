/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/common/builtineffectmodel.h"

namespace au::effects {
class LimiterViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    LimiterViewModel(QObject* parent = nullptr);

private:
    void doReload() override;
};
}
