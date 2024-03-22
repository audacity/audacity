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
#ifndef MU_API_NAVIGATIONAPI_H
#define MU_API_NAVIGATIONAPI_H

#include <QString>
#include <QJSValue>

#include "api/apiobject.h"
#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "ui/inavigationcontroller.h"

namespace mu::api {
class NavigationApi : public ApiObject
{
    Q_OBJECT

    INJECT(actions::IActionsDispatcher, dispatcher)
    INJECT(ui::INavigationController, navigation)

public:
    explicit NavigationApi(IApiEngine* e);
    ~NavigationApi();

    Q_INVOKABLE void nextPanel();
    Q_INVOKABLE void prevPanel();
    Q_INVOKABLE void right();
    Q_INVOKABLE void left();
    Q_INVOKABLE void up();
    Q_INVOKABLE void down();
    Q_INVOKABLE void escape();
    Q_INVOKABLE bool goToControl(const QString& section, const QString& panel, const QJSValue& controlNameOrIndex);
    Q_INVOKABLE void trigger();
    Q_INVOKABLE bool triggerControl(const QString& section, const QString& panel, const QJSValue& controlNameOrIndex);

    Q_INVOKABLE QString activeSection() const;
    Q_INVOKABLE QString activePanel() const;
    Q_INVOKABLE QString activeControl() const;

    Q_INVOKABLE void dump() const;
};
}

#endif // MU_API_NAVIGATIONAPI_H
