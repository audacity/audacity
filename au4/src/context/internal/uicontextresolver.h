/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef AU_CONTEXT_UICONTEXTRESOLVER_H
#define AU_CONTEXT_UICONTEXTRESOLVER_H

#include "../iuicontextresolver.h"
#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "iinteractive.h"
#include "../iglobalcontext.h"
#include "ui/inavigationcontroller.h"

#ifdef AU_BUILD_PLAYBACK_MODULE
#include "playback/iplaybackcontroller.h"
#endif

namespace au::context {
class UiContextResolver : public muse::ui::IUiContextResolver, public muse::async::Asyncable
{
    INJECT(muse::IInteractive, interactive)
    INJECT(IGlobalContext, globalContext)
    INJECT(muse::ui::INavigationController, navigationController)

#ifdef AU_BUILD_PLAYBACK_MODULE
    INJECT(playback::IPlaybackController, playbackController)
#endif
public:
    UiContextResolver() = default;

    void init();

    muse::ui::UiContext currentUiContext() const override;
    muse::async::Notification currentUiContextChanged() const override;

    bool match(const muse::ui::UiContext& currentCtx, const muse::ui::UiContext& actCtx) const override;
    bool matchWithCurrent(const muse::ui::UiContext& ctx) const override;

    bool isShortcutContextAllowed(const std::string& scContext) const override;

private:
    void notifyAboutContextChanged();

    muse::async::Notification m_currentUiContextChanged;
};
}

#endif // AU_CONTEXT_UICONTEXTRESOLVER_H
