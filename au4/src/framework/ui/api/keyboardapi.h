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
#ifndef MU_API_KEYBOARDAPI_H
#define MU_API_KEYBOARDAPI_H

#include "api/apiobject.h"

#include "modularity/ioc.h"
#include "shortcuts/ishortcutscontroller.h"
#include "ui/imainwindow.h"

namespace mu::api {
class KeyboardApi : public ApiObject
{
    Q_OBJECT

    INJECT(shortcuts::IShortcutsController, shortcutsController)
    INJECT(ui::IMainWindow, mainWindow)

public:
    explicit KeyboardApi(IApiEngine* e);

    Q_INVOKABLE void key(const QString& key);
    Q_INVOKABLE void repeatKey(const QString& key, int count);
    Q_INVOKABLE void text(const QString& text);
};
}

#endif // MU_API_KEYBOARDAPI_H
