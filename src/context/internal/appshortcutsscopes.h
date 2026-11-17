/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "framework/global/modularity/ioc.h"
#include "framework/shortcuts_v2/ishortcutsresolver.h"
#include "framework/ui/inavigationcontroller.h"

#include "context/iuicontextresolver.h"

namespace au::context {
class AppShortcutsScopes : public muse::Contextable, public std::enable_shared_from_this<AppShortcutsScopes>
{
    muse::ContextInject<muse::shortcuts::IShortcutsResolver> shortcutsResolver{ this };
    muse::ContextInject<IUiContextResolver> uiContextResolver{ this };
    muse::ContextInject<muse::ui::INavigationController> navigationController{ this };

public:
    AppShortcutsScopes(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    using Condition = bool (AppShortcutsScopes::*)() const;

    bool isProjectPageShown() const;
    bool isProjectFocused() const;
    bool isTrackViewFocused() const;
};
}
