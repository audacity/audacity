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
#pragma once

#include <array>
#include <string>

#include "shortcuts/shortcutcontext.h"

#include "global/containers.h"

namespace au::context {
// common shortcuts (re declared for convenience)
static const std::string CTX_ANY = muse::shortcuts::CTX_ANY;
static const std::string CTX_PROJECT_OPENED = muse::shortcuts::CTX_PROJECT_OPENED;
static const std::string CTX_PROJECT_FOCUSED = muse::shortcuts::CTX_PROJECT_FOCUSED;

//! NOTE special context for navigation shortcuts because the notation has its own navigation system
static const std::string CTX_NOT_PROJECT_FOCUSED = muse::shortcuts::CTX_NOT_PROJECT_FOCUSED;

class ShortcutContextPriority : public muse::shortcuts::IShortcutContextPriority
{
public:

    bool hasLowerPriorityThan(const std::string& ctx1, const std::string& ctx2) const override
    {
        static const std::array<std::string, 7> CONTEXTS_BY_INCREASING_PRIORITY {
            CTX_ANY,

            CTX_PROJECT_OPENED,
            CTX_NOT_PROJECT_FOCUSED,
            CTX_PROJECT_FOCUSED,
        };

        size_t index1 = muse::indexOf(CONTEXTS_BY_INCREASING_PRIORITY, ctx1);
        size_t index2 = muse::indexOf(CONTEXTS_BY_INCREASING_PRIORITY, ctx2);

        return index1 < index2;
    }
};
}
