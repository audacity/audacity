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

#include <string>

#include "framework/shortcuts/shortcutcontext.h"

namespace au::context {
// common shortcuts (re declared for convenience)
static const std::string CTX_DISABLED = muse::shortcuts::CTX_DISABLED;
static const std::string CTX_ANY = muse::shortcuts::CTX_ANY;
static const std::string CTX_PROJECT_OPENED = muse::shortcuts::CTX_PROJECT_OPENED;
static const std::string CTX_PROJECT_FOCUSED = muse::shortcuts::CTX_PROJECT_FOCUSED;

//! NOTE special context for navigation shortcuts because the notation has its own navigation system
static const std::string CTX_NOT_PROJECT_FOCUSED = muse::shortcuts::CTX_NOT_PROJECT_FOCUSED;
}
