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
#ifndef MU_CONTEXT_SHORTCUTCONTEXT_H
#define MU_CONTEXT_SHORTCUTCONTEXT_H

#include <array>
#include <string>

#include "global/containers.h"

namespace mu::context {
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

inline bool shortcutContextHasLowerPriorityThan(const std::string& ctx1, const std::string& ctx2)
{
    static const std::array<std::string, 7> CONTEXTS_BY_INCREASING_PRIORITY {
        CTX_ANY,

        CTX_NOTATION_OPENED,
        CTX_NOT_NOTATION_FOCUSED,
        CTX_NOTATION_FOCUSED,

        CTX_NOTATION_NOT_NOTE_INPUT_STAFF_TAB,
        CTX_NOTATION_NOTE_INPUT_STAFF_TAB,

        CTX_NOTATION_TEXT_EDITING
    };

    size_t index1 = mu::indexOf(CONTEXTS_BY_INCREASING_PRIORITY, ctx1);
    size_t index2 = mu::indexOf(CONTEXTS_BY_INCREASING_PRIORITY, ctx2);

    return index1 < index2;
}
}

#endif // MU_CONTEXT_SHORTCUTCONTEXT_H
