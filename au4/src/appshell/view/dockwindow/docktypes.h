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

#ifndef MU_DOCK_DOCKTYPES_H
#define MU_DOCK_DOCKTYPES_H

#include <QObject>
#include <QVariant>

namespace mu::dock {
inline const char* CONTEXT_MENU_MODEL_PROPERTY("contextMenuModel");
inline const char* DOCK_PANEL_PROPERTY("dockPanel");

//! NOTE: need to be synchronized with Window shadow(see DockFloatingWindow margins)
inline constexpr int DOCK_WINDOW_SHADOW(8);

enum class DockType {
    Undefined = -1,
    Panel,
    ToolBar,
    DockingHolder,
    StatusBar,
    Central
};

class DockLocation
{
    Q_GADGET

public:
    enum Location {
        Undefined,
        Left,
        Right,
        Center,
        Top,
        Bottom
    };

    Q_ENUM(Location)
};

using Location = DockLocation::Location;

struct DockProperties
{
    DockType type = DockType::Undefined;
    Location location = Location::Undefined;
    bool floatable = false;
    bool closable = false;
    bool resizable = false;
    bool separatorsVisible = false;
    bool selected = false;
    QRect highlightingRect;

    bool isValid() const
    {
        return type != DockType::Undefined;
    }
};

inline void writePropertiesToObject(const DockProperties& properties, QObject& obj)
{
    QObject* propertiesObj = obj.findChild<QObject*>("properties");
    if (!propertiesObj) {
        propertiesObj = new QObject(&obj);
        propertiesObj->setObjectName("properties");
    }

    propertiesObj->setProperty("dockType", static_cast<int>(properties.type));
    propertiesObj->setProperty("location", static_cast<int>(properties.location));
    propertiesObj->setProperty("floatable", properties.floatable);
    propertiesObj->setProperty("closable", properties.closable);
    propertiesObj->setProperty("resizable", properties.resizable);
    propertiesObj->setProperty("separatorsVisible", properties.separatorsVisible);
    propertiesObj->setProperty("highlightingRect", properties.highlightingRect);
}

inline DockProperties readPropertiesFromObject(const QObject* obj)
{
    if (!obj) {
        return DockProperties();
    }

    QObject* properties = obj->findChild<QObject*>("properties");
    if (!properties) {
        return DockProperties();
    }

    DockProperties result;
    result.type = static_cast<DockType>(properties->property("dockType").toInt());
    result.location = static_cast<Location>(properties->property("location").toInt());
    result.floatable = properties->property("floatable").toBool();
    result.closable = properties->property("closable").toBool();
    result.resizable = properties->property("resizable").toBool();
    result.separatorsVisible = properties->property("separatorsVisible").toBool();
    result.highlightingRect = properties->property("highlightingRect").toRect();

    return result;
}
}

#endif // MU_DOCK_DOCKTYPES_H
