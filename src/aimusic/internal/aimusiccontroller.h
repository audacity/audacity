/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/secs.h"
#include "framework/interactive/iinteractive.h"

#include "trackedit/iselectioncontroller.h"
#include "trackedit/iprojecthistory.h"
#include "context/iglobalcontext.h"

namespace au::aimusic {
class AiMusicController : public muse::actions::Actionable, public muse::Contextable
{
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<trackedit::ISelectionController> selectionController { this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };

public:
    AiMusicController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    void analyzeSelection();
};
}
