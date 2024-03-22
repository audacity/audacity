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
#include "accessibilityconfiguration.h"

#include <QAccessible>

using namespace mu::accessibility;

class AccessibilityActivationObserver : public QAccessible::ActivationObserver
{
public:
    AccessibilityActivationObserver()
    {
        m_isAccessibilityActive = QAccessible::isActive();
    }

    bool isAccessibilityActive() const
    {
        return m_isAccessibilityActive;
    }

    void accessibilityActiveChanged(bool active) override
    {
        m_isAccessibilityActive = active;
    }

private:
    bool m_isAccessibilityActive = false;
};

AccessibilityActivationObserver* s_accessibilityActivationObserver = nullptr;

AccessibilityConfiguration::~AccessibilityConfiguration()
{
    QAccessible::installActivationObserver(nullptr);
    delete s_accessibilityActivationObserver;
}

void AccessibilityConfiguration::init()
{
    s_accessibilityActivationObserver = new AccessibilityActivationObserver();

    QAccessible::installActivationObserver(s_accessibilityActivationObserver);

    m_inited = true;
}

bool AccessibilityConfiguration::enabled() const
{
    if (!m_inited) {
        return false;
    }

    if (!navigationController()) {
        return false;
    }

    if (!active()) {
        return false;
    }

    //! NOTE Accessibility available if navigation is used
    return navigationController()->activeSection() != nullptr;
}

bool AccessibilityConfiguration::active() const
{
    if (!m_inited) {
        return false;
    }

    return s_accessibilityActivationObserver->isAccessibilityActive();
}
