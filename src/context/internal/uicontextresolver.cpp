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
#include "uicontextresolver.h"

#include "shortcutcontext.h"
#include "log.h"

using namespace au::context;
using namespace muse;
using namespace muse::ui;

static const Uri HOME_PAGE_URI("musescore://home");
static const Uri PROJECT_PAGE_URI("audacity://project");

//! TODO AU4: this should point to sth like ProjectView
//! but we don't have that yet, so binding it to
//! area that's being focused when opening a project
static const QString PROJECT_NAVIGATION_PANEL("MainToolBar");

void UiContextResolver::init()
{
    interactive()->currentUri().ch.onReceive(this, [this](const Uri&) {
        notifyAboutContextChanged();
    });

#ifdef AU_BUILD_PLAYBACK_MODULE
    playbackController()->isPlayingChanged().onNotify(this, [this]() {
        notifyAboutContextChanged();
    });
#endif

    //! TODO AU4
    // globalContext()->currentProjectChanged().onNotify(this, [this]() {
    //     auto project = globalContext()->currentProject();
    //     if (notation) {
    //         notation->interaction()->selectionChanged().onNotify(this, [this]() {
    //             notifyAboutContextChanged();
    //         });

    //         notation->interaction()->textEditingStarted().onNotify(this, [this]() {
    //             notifyAboutContextChanged();
    //         });

    //         notation->interaction()->textEditingEnded().onReceive(this, [this](engraving::TextBase*) {
    //             notifyAboutContextChanged();
    //         });

    //         notation->undoStack()->stackChanged().onNotify(this, [this]() {
    //             notifyAboutContextChanged();
    //         });

    //         notation->interaction()->noteInput()->noteInputStarted().onNotify(this, [this]() {
    //             notifyAboutContextChanged();
    //         });

    //         notation->interaction()->noteInput()->noteInputEnded().onNotify(this, [this]() {
    //             notifyAboutContextChanged();
    //         });
    //     }
    //     notifyAboutContextChanged();
    // });

    navigationController()->navigationChanged().onNotify(this, [this]() {
        notifyAboutContextChanged();
    });
}

void UiContextResolver::notifyAboutContextChanged()
{
    m_currentUiContextChanged.notify();
}

UiContext UiContextResolver::currentUiContext() const
{
    TRACEFUNC;
    Uri currentUri = interactive()->currentUri().val;

#ifdef MUE_BUILD_DIAGNOSTICS_MODULE
    currentUri = diagnostics::diagnosticCurrentUri(interactive()->stack());
#endif

    if (currentUri == HOME_PAGE_URI) {
        return context::UiCtxHomeOpened;
    }

    if (currentUri == PROJECT_PAGE_URI) {
        auto project = globalContext()->currentProject();
        if (!project) {
            //! NOTE The notation page is open, but the notation itself is not loaded - we consider that the notation is not open.
            //! We need to think, maybe we need a separate value for this case.
            return context::UiCtxUnknown;
        }

        INavigationPanel* activePanel = navigationController()->activePanel();
        if (activePanel) {
            if (activePanel->name() == PROJECT_NAVIGATION_PANEL) {
                return context::UiCtxProjectFocused;
            }
        }

        return context::UiCtxProjectOpened;
    }

    return context::UiCtxUnknown;
}

bool UiContextResolver::match(const ui::UiContext& currentCtx, const ui::UiContext& actCtx) const
{
    if (actCtx == context::UiCtxAny) {
        return true;
    }

    //! NOTE If the current context is `UiCtxProjectFocused`, then we allow `UiCtxProjectOpened` too
    if (currentCtx == context::UiCtxProjectFocused && actCtx == context::UiCtxProjectOpened) {
        return true;
    }

    return currentCtx == actCtx;
}

bool UiContextResolver::matchWithCurrent(const UiContext& ctx) const
{
    if (ctx == ui::UiCtxAny) {
        return true;
    }

    UiContext currentCtx = currentUiContext();
    return match(currentCtx, ctx);
}

async::Notification UiContextResolver::currentUiContextChanged() const
{
    return m_currentUiContextChanged;
}

bool UiContextResolver::isShortcutContextAllowed(const std::string& scContext) const
{
    //! NOTE If (when) there are many different contexts here,
    //! then the implementation of this method will need to be changed
    //! so that it does not become spaghetti-code.
    //! It would be nice if this context as part of the UI context,
    //! for this we should complicate the implementation of the UI context,
    //! probably make a tree, for example:
    //! ProjectOpened
    //!     ProjectFocused

    if (CTX_PROJECT_OPENED == scContext) {
        return matchWithCurrent(context::UiCtxProjectOpened);
    } else if (CTX_PROJECT_FOCUSED == scContext) {
        return matchWithCurrent(context::UiCtxProjectFocused);
    } else if (CTX_NOT_PROJECT_FOCUSED == scContext) {
        return !matchWithCurrent(context::UiCtxProjectFocused);
    }

    IF_ASSERT_FAILED(CTX_ANY == scContext) {
        return true;
    }
    return true;
}
