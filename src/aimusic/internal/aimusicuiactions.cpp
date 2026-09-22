/*
 * Audacity: A Digital Audio Editor
 */
#include "aimusicuiactions.h"

#include "framework/global/types/translatablestring.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"

#include "aimusiccontroller.h"

using namespace au::aimusic;
using namespace muse;
using namespace muse::ui;

static const UiActionList aiMusicActions {
    UiAction("ai-music-analyze-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             muse::TranslatableString("action", "AI Music: Analyze selection"),
             muse::TranslatableString("action_description", "Run the local AI Music Studio analysis helper on the selected audio"))
};

AiMusicUiActions::AiMusicUiActions(const modularity::ContextPtr& ctx, std::shared_ptr<AiMusicController> controller)
    : Contextable(ctx), m_controller(std::move(controller))
{
}

const UiActionList& AiMusicUiActions::actionsList() const
{
    return aiMusicActions;
}

bool AiMusicUiActions::actionEnabled(const UiAction& action) const
{
    return m_controller->canReceiveAction(action.code);
}

muse::async::Channel<muse::actions::ActionCodeList> AiMusicUiActions::actionEnabledChanged() const
{
    return {};
}

bool AiMusicUiActions::actionChecked(const UiAction&) const
{
    return false;
}

muse::async::Channel<muse::actions::ActionCodeList> AiMusicUiActions::actionCheckedChanged() const
{
    return {};
}
