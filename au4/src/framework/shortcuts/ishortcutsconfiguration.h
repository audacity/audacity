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
#ifndef MU_SHORTCUTS_ISHORTCUTSCONFIGURATION_H
#define MU_SHORTCUTS_ISHORTCUTSCONFIGURATION_H

#include "modularity/imoduleinterface.h"
#include "io/path.h"
#include "types/retval.h"

namespace mu::shortcuts {
class IShortcutsConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IShortcutsConfiguration)

public:
    virtual ~IShortcutsConfiguration() = default;

    virtual QString currentKeyboardLayout() const = 0;
    virtual void setCurrentKeyboardLayout(const QString& layout) = 0;

    virtual io::path_t shortcutsUserAppDataPath() const = 0;
    virtual io::path_t shortcutsAppDataPath() const = 0;

    virtual io::path_t midiMappingUserAppDataPath() const = 0;

    virtual bool advanceToNextNoteOnKeyRelease() const = 0;
    virtual void setAdvanceToNextNoteOnKeyRelease(bool value) = 0;
};
}

#endif // MU_SHORTCUTS_ISHORTCUTSCONFIGURATION_H
