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
#ifndef MU_SHORTCUTS_SHORTCUTSCONFIGURATION_H
#define MU_SHORTCUTS_SHORTCUTSCONFIGURATION_H

#include "modularity/ioc.h"
#include "iglobalconfiguration.h"
#include "ishortcutsconfiguration.h"
#include "async/asyncable.h"

namespace mu::shortcuts {
class ShortcutsConfiguration : public IShortcutsConfiguration, public async::Asyncable
{
    INJECT(IGlobalConfiguration, globalConfiguration)

public:
    void init();

    QString currentKeyboardLayout() const override;
    void setCurrentKeyboardLayout(const QString& layout) override;

    io::path_t shortcutsUserAppDataPath() const override;
    io::path_t shortcutsAppDataPath() const override;

    io::path_t midiMappingUserAppDataPath() const override;

    bool advanceToNextNoteOnKeyRelease() const override;
    void setAdvanceToNextNoteOnKeyRelease(bool value) override;
};
}

#endif // MU_SHORTCUTS_SHORTCUTSCONTROLLER_H
