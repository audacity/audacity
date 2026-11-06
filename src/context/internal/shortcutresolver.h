/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/modularity/ioc.h"
#include "framework/shortcuts_v2/ishortcutsresolver.h"
#include "framework/ui/inavigationcontroller.h"

#include "context/iuicontextresolver.h"

namespace au::context {
class ShortcutResolver : public muse::shortcuts::IShortcutsResolver, public muse::Contextable
{
    muse::ContextInject<IUiContextResolver> uiContextResolver{ this };
    muse::ContextInject<muse::ui::INavigationController> navigationController{ this };

public:
    ShortcutResolver(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    muse::shortcuts::Shortcut selectActive(const muse::shortcuts::ShortcutList& list) const override;

    muse::TranslatableString scopeTitle(const std::string& scopeCode) const override;

private:
    int scopePriority(const std::string& scope) const;
    bool isProjectPageShown() const;
    bool isProjectFocused() const;
    bool isTrackViewFocused() const;
};
}
