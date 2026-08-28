/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/api/apiobject.h"
#include "framework/global/modularity/ioc.h"

#include "../ieffectexecutionscenario.h"
#include "../ieffectsprovider.h"

namespace au::effects {
//! Scripting/testflow API exposed as "Audacity.Effects".
class EffectsApi : public muse::api::ApiObject
{
    Q_OBJECT

    muse::ContextInject<IEffectExecutionScenario> effectExecutionScenario = { this };
    muse::GlobalInject<IEffectsProvider> effectsProvider;

public:
    explicit EffectsApi(muse::api::IApiEngine* e);

    //! True while an effect launched via effects/apply is still validating or
    //! being applied. Applying an effect is asynchronous (see #11746), so
    //! scripts poll this to know when they can proceed to the next step.
    Q_INVOKABLE bool isApplying() const;

    //! True once an effect with this title is validated and loadable. On a
    //! blank-config first run a plugin is validated in the background, so its
    //! title only appears here once validation finishes; scripts poll this
    //! before applying a plugin by title.
    Q_INVOKABLE bool isAvailable(const QString& title) const;
};
}
