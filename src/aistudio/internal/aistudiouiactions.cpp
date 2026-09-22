/*
 * Audacity: A Digital Audio Editor
 */
#include "aistudiouiactions.h"

#include "context/shortcutcontext.h"
#include "context/uicontext.h"
#include "global/types/translatablestring.h"

#include "aistudiocontroller.h"

using namespace au::aistudio;
using namespace muse;
using namespace muse::ui;

static const UiActionList aiStudioActions {
    UiAction("ai.openJobs",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "AI Studio"),
             TranslatableString("action_description", "Open the AI Studio panel"))
};

AIStudioUiActions::AIStudioUiActions(const modularity::ContextPtr& ctx, std::shared_ptr<AIStudioController> controller)
    : Contextable(ctx), m_controller(std::move(controller))
{
}

const UiActionList& AIStudioUiActions::actionsList() const { return aiStudioActions; }
bool AIStudioUiActions::actionEnabled(const UiAction& action) const { return m_controller->canReceiveAction(action.code); }
muse::async::Channel<muse::actions::ActionCodeList> AIStudioUiActions::actionEnabledChanged() const { return {}; }
bool AIStudioUiActions::actionChecked(const UiAction&) const { return false; }
muse::async::Channel<muse::actions::ActionCodeList> AIStudioUiActions::actionCheckedChanged() const { return {}; }
