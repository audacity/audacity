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
#ifndef MU_UI_NAVIGATIONEVENT_H
#define MU_UI_NAVIGATIONEVENT_H

#include <QObject>

#include "ui/inavigation.h"

namespace mu::ui {
class NavigationEvent
{
    Q_GADGET
    Q_PROPERTY(Type type READ type CONSTANT)
    Q_PROPERTY(bool accepted READ accepted WRITE setAccepted)

public:
    NavigationEvent(INavigation::EventPtr event = nullptr);

    //! NOTE Please sync with INavigation::Event::Type
    enum Type {
        Undefined = 0,
        Left,
        Right,
        Up,
        Down,
        Trigger,
        Escape,
        AboutActive
    };
    Q_ENUM(Type)

    void setEvent(INavigation::EventPtr event);

    Type type() const;
    bool accepted() const;
    void setAccepted(bool accepted);

    Q_INVOKABLE QVariant data(const QString& key) const;
    Q_INVOKABLE void setData(const QString& key, const QVariant& val);

private:
    INavigation::EventPtr m_event;
};
}
Q_DECLARE_METATYPE(mu::ui::NavigationEvent)

#endif // MU_UI_NAVIGATIONEVENT_H
