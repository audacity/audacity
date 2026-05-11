/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "imissingeffectchecker.h"
#include "ieffectsprovider.h"
#include "irealtimeeffectservice.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/interactive/iinteractive.h"

namespace au::effects {
class MissingEffectChecker : public IMissingEffectChecker, public muse::Contextable, public muse::async::Asyncable
{
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::GlobalInject<IEffectsProvider> effectsProvider;
    muse::ContextInject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };

public:
    MissingEffectChecker(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void warnIfEffectsMissing() override;
};
}
