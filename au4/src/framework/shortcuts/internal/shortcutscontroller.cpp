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
#include "shortcutscontroller.h"

#include "context/shortcutcontext.h"

#include "log.h"

using namespace mu::shortcuts;
using namespace mu::actions;

void ShortcutsController::init()
{
    interactiveProvider()->currentUri().ch.onReceive(this, [this](const Uri&) {
        //! NOTE: enable process shortcuts only for non-widget objects
        shortcutsRegister()->setActive(!interactiveProvider()->topWindowIsWidget());
    });
}

void ShortcutsController::activate(const std::string& sequence)
{
    LOGD() << sequence;

    ActionCode actionCode = resolveAction(sequence);

    if (!actionCode.empty()) {
        dispatcher()->dispatch(actionCode);
    }
}

bool ShortcutsController::isRegistered(const std::string& sequence) const
{
    return shortcutsRegister()->isRegistered(sequence);
}

ActionCode ShortcutsController::resolveAction(const std::string& sequence) const
{
    ShortcutList shortcutsForSequence = shortcutsRegister()->shortcutsForSequence(sequence);
    IF_ASSERT_FAILED(!shortcutsForSequence.empty()) {
        return ActionCode();
    }

    ShortcutList allowedShortcuts;

    for (const Shortcut& sc : shortcutsForSequence) {
        //! NOTE Check if the shortcut itself is allowed
        if (!uiContextResolver()->isShortcutContextAllowed(sc.context)) {
            continue;
        }

        //! NOTE Check if the action is allowed
        ui::UiActionState st = aregister()->actionState(sc.action);
        if (!st.enabled) {
            continue;
        }

        allowedShortcuts.push_back(sc);
    }

    allowedShortcuts.sort([](const Shortcut& f, const Shortcut& s) {
        return context::shortcutContextHasLowerPriorityThan(f.context, s.context);
    });

    return !allowedShortcuts.empty() ? allowedShortcuts.back().action : ActionCode();
}
