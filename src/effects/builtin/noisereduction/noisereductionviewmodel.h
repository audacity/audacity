/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

#include "effects/effects_base/ieffectsprovider.h"

class NoiseReductionBase;

namespace au::effects {
class NoiseReductionViewModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    NoiseReductionViewModel() = default;

    bool isApplyAllowed() const;
    void setIsApplyAllowed(bool isApplyAllowed);

signals:
    void isApplyAllowedChanged();

private:
    void doReload() override;

    NoiseReductionBase* effect() const;

    bool m_isApplyAllowed = false;
};
}
