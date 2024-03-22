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
#include "navigationuiactions.h"

//! TODO AU4
// #include "context/uicontext.h"
namespace mu::context {
//! NOTE Determines where to be, what the user is doing

// common ui (re declared for convenience)
static constexpr ui::UiContext UiCtxUnknown = ui::UiCtxUnknown;
static constexpr ui::UiContext UiCtxAny = ui::UiCtxAny;

// pages
static constexpr ui::UiContext UiCtxNotationOpened = "UiCtxNotationOpened";
static constexpr ui::UiContext UiCtxHomeOpened = "UiCtxHomeOpened";

// notation detail
static constexpr ui::UiContext UiCtxNotationFocused = "UiCtxNotationFocused";

static const std::string CTX_ANY("any");
static const std::string CTX_NOTATION_OPENED("notation-opened");
static const std::string CTX_NOTATION_FOCUSED("notation-focused");

//! NOTE special context for navigation shortcuts because the notation has its own navigation system
static const std::string CTX_NOT_NOTATION_FOCUSED("not-notation-focused");

/// We're not [in note input on a TAB staff] (i.e. either not in note input mode, or in note input mode but not on a TAB staff)
static const std::string CTX_NOTATION_NOT_NOTE_INPUT_STAFF_TAB("notation-not-note-input-staff-tab");
/// We're in note input on a TAB staff
static const std::string CTX_NOTATION_NOTE_INPUT_STAFF_TAB("notation-note-input-staff-tab");

static const std::string CTX_NOTATION_TEXT_EDITING("notation-text-editing");
}

using namespace mu::ui;
using namespace mu::actions;

const UiActionList NavigationUiActions::m_actions = {
    UiAction("nav-dev-show-controls",
             mu::context::UiCtxAny,
             mu::context::CTX_ANY
             ),
    UiAction("nav-next-section",
             mu::context::UiCtxAny,
             mu::context::CTX_ANY
             ),
    UiAction("nav-prev-section",
             mu::context::UiCtxAny,
             mu::context::CTX_ANY
             ),
    UiAction("nav-next-panel",
             mu::context::UiCtxAny,
             mu::context::CTX_ANY
             ),
    UiAction("nav-prev-panel",
             mu::context::UiCtxAny,
             mu::context::CTX_ANY
             ),
    UiAction("nav-next-tab",
             mu::context::UiCtxAny,
             mu::context::CTX_ANY
             ),
    UiAction("nav-prev-tab",
             mu::context::UiCtxAny,
             mu::context::CTX_ANY
             ),
    UiAction("nav-right",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-left",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-up",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-down",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-escape",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-trigger-control",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-first-control",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-last-control",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-nextrow-control",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             ),
    UiAction("nav-prevrow-control",
             mu::context::UiCtxAny,
             mu::context::CTX_NOT_NOTATION_FOCUSED
             )
};

const UiActionList& NavigationUiActions::actionsList() const
{
    return m_actions;
}

bool NavigationUiActions::actionEnabled(const UiAction&) const
{
    return true;
}

mu::async::Channel<ActionCodeList> NavigationUiActions::actionEnabledChanged() const
{
    static async::Channel<ActionCodeList> ch;
    return ch;
}

bool NavigationUiActions::actionChecked(const UiAction&) const
{
    return false;
}

mu::async::Channel<ActionCodeList> NavigationUiActions::actionCheckedChanged() const
{
    static async::Channel<ActionCodeList> ch;
    return ch;
}
