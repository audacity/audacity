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
#include "navigationpopuppanel.h"

using namespace mu::ui;

NavigationPopupPanel::NavigationPopupPanel(QObject* parent)
    : NavigationPanel(parent)
{
}

INavigationControl* NavigationPopupPanel::parentControl() const
{
    return m_parentControl;
}

NavigationControl* NavigationPopupPanel::parentControl_property() const
{
    return dynamic_cast<NavigationControl*>(m_parentControl);
}

void NavigationPopupPanel::setParentControl(NavigationControl* parentControl)
{
    setParentControl(static_cast<INavigationControl*>(parentControl));
}

void NavigationPopupPanel::setParentControl(INavigationControl* parentControl)
{
    if (m_parentControl == parentControl) {
        return;
    }

    m_parentControl = parentControl;
    emit parentControlChanged();

    if (m_parentControl && m_parentControl->panel()) {
        setSection(m_parentControl->panel()->section());
    } else {
        setSection(nullptr);
    }
}
