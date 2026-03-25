/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramuiactions.h"
#include "spectrogramtypes.h"

#include "context/shortcutcontext.h"
#include "context/uicontext.h"

#include "framework/global/types/translatablestring.h"

namespace au::spectrogram {
namespace {
static const muse::ui::UiActionList STATIC_ACTIONS = {
    muse::ui::UiAction(TRACK_SPECTROGRAM_SETTINGS_ACTION,
                       au::context::UiCtxAny,
                       au::context::CTX_ANY,
                       muse::TranslatableString("action", "Spectrogram settings…"),
                       muse::TranslatableString("action", "Spectrogram settings…")
                       )
};
}

SpectrogramUiActions::SpectrogramUiActions(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx), m_actions(STATIC_ACTIONS)
{
}

const muse::ui::UiActionList& SpectrogramUiActions::actionsList() const
{
    return m_actions;
}

bool SpectrogramUiActions::actionEnabled(const muse::ui::UiAction&) const
{
    return true;
}

bool SpectrogramUiActions::actionChecked(const muse::ui::UiAction&) const
{
    return true;
}

muse::async::Channel<muse::actions::ActionCodeList> SpectrogramUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<muse::actions::ActionCodeList> SpectrogramUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
}
