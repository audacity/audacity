/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class GraphicEq;
class GraphicEqViewModel : public AbstractEffectModel
{
    Q_OBJECT

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    GraphicEqViewModel() = default;

    GraphicEq* effect() const;

private:
    void doReload() override;
};
}
