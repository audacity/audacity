/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"

#include "effects/effects_base/ieffectsprovider.h"

class LoudnessBase;
namespace au::effects {
class NormalizeLoudnessViewModel : public AbstractEffectModel
{
    Q_OBJECT

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    NormalizeLoudnessViewModel() = default;

private:
    void doReload() override;

    LoudnessBase* effect() const;
};
}
