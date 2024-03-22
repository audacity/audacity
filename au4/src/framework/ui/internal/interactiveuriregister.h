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
#ifndef MU_UI_INTERACTIVEURIREGISTER_H
#define MU_UI_INTERACTIVEURIREGISTER_H

#include "iinteractiveuriregister.h"

template<>
struct std::hash<mu::Uri>
{
    std::size_t operator()(const mu::Uri& uri) const noexcept
    {
        return std::hash<std::string> {}(uri.toString());
    }
};

namespace mu::ui {
class InteractiveUriRegister : public IInteractiveUriRegister
{
public:
    void registerUri(const Uri& uri, const ContainerMeta& meta) override;
    ContainerMeta meta(const Uri& uri) const override;

private:
    std::unordered_map<Uri, ContainerMeta> m_uriMap;
};
}

#endif // MU_UI_INTERACTIVEURIREGISTER_H
