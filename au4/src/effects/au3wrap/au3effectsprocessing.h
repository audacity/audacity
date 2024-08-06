/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/types/string.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"

#include "libraries/lib-effects/Effect.h"

#include "effects/ieffectsprocessing.h"

namespace au::effects {
class Au3EffectsProcessing : public IEffectsProcessing
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    void process(const muse::String& effectId) override;

    void regEffect(const muse::String& effectId, Effect* effect) override;
    void unregEffect(const muse::String& effectId) override;

private:
    AudacityProject& projectRef() const;

    void onApplyEffect(const muse::actions::ActionData& args) const;

    std::map<muse::String, Effect*> m_effectsMap;
};
}
