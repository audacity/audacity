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
#include "navigationapi.h"

#include "log.h"

using namespace mu::api;
using namespace mu::ui;

NavigationApi::NavigationApi(IApiEngine* e)
    : ApiObject(e)
{
}

NavigationApi::~NavigationApi()
{
}

void NavigationApi::nextPanel()
{
    dispatcher()->dispatch("nav-next-panel");
}

void NavigationApi::prevPanel()
{
    dispatcher()->dispatch("nav-prev-panel");
}

void NavigationApi::right()
{
    dispatcher()->dispatch("nav-right");
}

void NavigationApi::left()
{
    dispatcher()->dispatch("nav-left");
}

void NavigationApi::up()
{
    dispatcher()->dispatch("nav-up");
}

void NavigationApi::down()
{
    dispatcher()->dispatch("nav-down");
}

void NavigationApi::escape()
{
    dispatcher()->dispatch("nav-escape");
}

bool NavigationApi::goToControl(const QString& section, const QString& panel, const QJSValue& controlNameOrIndex)
{
    bool ok = false;
    if (controlNameOrIndex.isString()) {
        ok = navigation()->requestActivateByName(section.toStdString(), panel.toStdString(), controlNameOrIndex.toString().toStdString());
    } else if (controlNameOrIndex.isArray()) {
        if (controlNameOrIndex.property("length").toInt() == 2) {
            INavigation::Index idx;
            idx.row = controlNameOrIndex.property(0).toInt();
            idx.column = controlNameOrIndex.property(1).toInt();
            ok = navigation()->requestActivateByIndex(section.toStdString(), panel.toStdString(), idx);
        } else {
            LOGE() << "bad argument `control`: " << controlNameOrIndex.toString();
            ok = false;
        }
    } else {
        LOGE() << "bad argument `control`: " << controlNameOrIndex.toString();
        ok = false;
    }

    return ok;
}

void NavigationApi::trigger()
{
    dispatcher()->dispatch("nav-trigger-control");
}

bool NavigationApi::triggerControl(const QString& section, const QString& panel, const QJSValue& controlNameOrIndex)
{
    bool ok = goToControl(section, panel, controlNameOrIndex);
    if (ok) {
        trigger();
    }
    return ok;
}

QString NavigationApi::activeSection() const
{
    INavigationSection* sec = navigation()->activeSection();
    return sec ? sec->name() : QString();
}

QString NavigationApi::activePanel() const
{
    INavigationPanel* p = navigation()->activePanel();
    return p ? p->name() : QString();
}

QString NavigationApi::activeControl() const
{
    INavigationControl* c = navigation()->activeControl();
    return c ? c->name() : QString();
}

void NavigationApi::dump() const
{
    navigation()->dump();
}
