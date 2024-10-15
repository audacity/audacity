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
#ifndef AU_CONTEXT_UICONTEXT_H
#define AU_CONTEXT_UICONTEXT_H

#include "ui/uiaction.h"

namespace au::context {
//! NOTE Determines where to be, what the user is doing

// common ui (re declared for convenience)
static constexpr muse::ui::UiContext UiCtxUnknown = muse::ui::UiCtxUnknown;
static constexpr muse::ui::UiContext UiCtxAny = muse::ui::UiCtxAny;

// pages
static constexpr muse::ui::UiContext UiCtxProjectOpened = "UiCtxProjectOpened";
static constexpr muse::ui::UiContext UiCtxHomeOpened = "UiCtxHomeOpened";

// project detail
static constexpr muse::ui::UiContext UiCtxProjectFocused = "UiCtxProjectFocused";
}

#endif // AU_CONTEXT_UICONTEXT_H
