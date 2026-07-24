/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "playback/iplaybackcontroller.h"
#include "actions/iactionsdispatcher.h"

#include "trackedit/iselectioncontroller.h"

#include "projectscene/view/timeline/timelinecontext.h"
#include "projectscene/view/timeline/snaptimeformatter.h"
#include "projectscene/view/playcursor/playcursorcontroller.h"
#include "projectscene/view/playcursor/playpositionactioncontroller.h"

namespace au::projectscene {
//! Test-only helper that reaches the private injects of TimelineContext and
//! PlayCursorController so the snapping/guideline logic can be exercised without
//! running their full init() machinery.
struct SnapTestAccess {
    static void wireContext(TimelineContext* ctx,
                            const std::shared_ptr<context::IGlobalContext>& globalContext,
                            const std::shared_ptr<playback::IPlayback>& playback)
    {
        ctx->globalContext.set(globalContext);
        ctx->playback.set(playback);
        ctx->m_snapTimeFormatter = std::make_shared<SnapTimeFormatter>(muse::modularity::globalCtx());
    }

    static void wireCursor(PlayCursorController* cursor,
                           const std::shared_ptr<context::IGlobalContext>& globalContext,
                           const std::shared_ptr<muse::actions::IActionsDispatcher>& dispatcher,
                           const std::shared_ptr<playback::IPlaybackController>& playbackController = nullptr)
    {
        cursor->globalContext.set(globalContext);
        cursor->dispatcher.set(dispatcher);
        if (playbackController) {
            cursor->playbackController.set(playbackController);
        }
    }

    static void wirePlayPosition(PlayPositionActionController* controller,
                                 const std::shared_ptr<context::IGlobalContext>& globalContext,
                                 const std::shared_ptr<muse::actions::IActionsDispatcher>& dispatcher,
                                 const std::shared_ptr<trackedit::ISelectionController>& selectionController)
    {
        controller->globalContext.set(globalContext);
        controller->dispatcher.set(dispatcher);
        controller->selectionController.set(selectionController);
    }
};
}
