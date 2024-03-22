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

#include "testing/environment.h"

#include "log.h"
#include "framework/accessibility/accessibilitymodule.h"
#include "framework/ui/uimodule.h"
#include "framework/global/globalmodule.h"

static mu::testing::SuiteEnvironment accessibility_senv(
{
    new mu::accessibility::AccessibilityModule()
},
    nullptr,
    []() {
    LOGI() << "accessibility tests suite post init";
}
    );
