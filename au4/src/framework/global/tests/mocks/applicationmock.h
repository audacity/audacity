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
#ifndef MU_GLOBAL_APPLICATIONMOCK_H
#define MU_GLOBAL_APPLICATIONMOCK_H

#include <gmock/gmock.h>

#include "global/iapplication.h"

namespace mu {
class ApplicationMock : public IApplication
{
public:
    MOCK_METHOD(void, setRunMode, (const RunMode&), (override));
    MOCK_METHOD(RunMode, runMode, (), (const, override));
    MOCK_METHOD(bool, noGui, (), (const, override));

    MOCK_METHOD(QWindow*, focusWindow, (), (const, override));

    MOCK_METHOD(bool, notify, (QObject*, QEvent*), (override));

    MOCK_METHOD(void, restart, (), (override));
};
}

#endif // MU_GLOBAL_APPLICATIONMOCK_H
