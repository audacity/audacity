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
#include "interactiveuriregister.h"

#include "log.h"

using namespace mu::ui;

void InteractiveUriRegister::registerUri(const Uri& uri, const ContainerMeta& meta)
{
    IF_ASSERT_FAILED(!mu::contains(m_uriMap, uri)) {
        LOGW() << "URI " << uri.toString() << " is already registered. Will be overridden.";
    }

    m_uriMap[uri] = meta;
}

ContainerMeta InteractiveUriRegister::meta(const Uri& uri) const
{
    if (!mu::contains(m_uriMap, uri)) {
        LOGW() << "URI " << uri.toString() << " is not registered";
        return ContainerMeta();
    }

    return m_uriMap.at(uri);
}
