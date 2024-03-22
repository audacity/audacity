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
#ifndef MU_UI_IINTERACTIVEURIREGISTER_H
#define MU_UI_IINTERACTIVEURIREGISTER_H

#include <type_traits>

#include "modularity/imoduleinterface.h"
#include "types/uri.h"
#include "uitypes.h"

namespace mu::ui {
class IInteractiveUriRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IInteractiveUriRegister)

public:
    virtual ~IInteractiveUriRegister() = default;

    virtual void registerUri(const Uri& uri, const ContainerMeta& meta) = 0;
    virtual ContainerMeta meta(const Uri& uri) const = 0;

    // useful
    void registerQmlUri(const Uri& uri, const QString& qmlPath)
    {
        registerUri(uri, ContainerMeta(ContainerType::Type::QmlDialog, qmlPath));
    }

    template<typename T>
    void registerWidgetUri(const Uri& uri)
    {
        static_assert(std::is_base_of<QWidget, T>::value, "T must derive from QWidget");
        registerUri(uri, ContainerMeta(ContainerType::Type::QWidgetDialog, qRegisterMetaType<T>(typeid(T).name())));
    }
};
}

#endif // MU_UI_IINTERACTIVEURIREGISTER_H
