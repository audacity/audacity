/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayer.h"
#include "actions/iactionsdispatcher.h"

#include "projectscene/view/timeline/timelinecontext.h"
#include "projectscene/view/timeline/snaptimeformatter.h"
#include "projectscene/view/playcursor/playcursorcontroller.h"

namespace au::projectscene {
//! Test-only helper that reaches the private injects of TimelineContext and
//! PlayCursorController so the snapping/guideline logic can be exercised without
//! running their full init() machinery.
struct SnapTestAccess {
    static void wireContext(TimelineContext* ctx,
                            const std::shared_ptr<context::IGlobalContext>& globalContext,
                            const std::shared_ptr<playback::IPlayer>& player)
    {
        ctx->globalContext.set(globalContext);
        ctx->player.set(player);
        ctx->m_snapTimeFormatter = std::make_shared<SnapTimeFormatter>(muse::modularity::globalCtx());
    }

    static void wireCursor(PlayCursorController* cursor,
                           const std::shared_ptr<context::IGlobalContext>& globalContext,
                           const std::shared_ptr<muse::actions::IActionsDispatcher>& dispatcher)
    {
        cursor->globalContext.set(globalContext);
        cursor->dispatcher.set(dispatcher);
    }
};
}
