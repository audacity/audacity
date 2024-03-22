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
#ifndef MU_SHORTCUTS_SHORTCUTSREGISTER_H
#define MU_SHORTCUTS_SHORTCUTSREGISTER_H

#include <unordered_map>

#include "../ishortcutsregister.h"
#include "modularity/ioc.h"
#include "ishortcutsconfiguration.h"
#include "ui/iuiactionsregister.h"
#include "async/asyncable.h"
#include "io/ifilesystem.h"
#include "multiinstances/imultiinstancesprovider.h"

namespace mu::deprecated {
class XmlReader;
class XmlWriter;
}

namespace mu::shortcuts {
class ShortcutsRegister : public IShortcutsRegister, public async::Asyncable
{
    INJECT(IShortcutsConfiguration, configuration)
    INJECT(io::IFileSystem, fileSystem)
    INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    INJECT(ui::IUiActionsRegister, uiactionsRegister)

public:
    ShortcutsRegister() = default;

    void init();

    void reload(bool onlyDef = false) override;

    const ShortcutList& shortcuts() const override;
    Ret setShortcuts(const ShortcutList& shortcuts) override;
    void resetShortcuts() override;
    async::Notification shortcutsChanged() const override;

    Ret setAdditionalShortcuts(const std::string& context, const ShortcutList& shortcuts) override;

    const Shortcut& shortcut(const std::string& actionCode) const override;
    const Shortcut& defaultShortcut(const std::string& actionCode) const override;

    bool isRegistered(const std::string& sequence) const override;
    ShortcutList shortcutsForSequence(const std::string& sequence) const override;

    Ret importFromFile(const io::path_t& filePath) override;
    Ret exportToFile(const io::path_t& filePath) const override;

    bool active() override;
    void setActive(bool active) override;
    async::Notification activeChanged() const override;

private:

    bool readFromFile(ShortcutList& shortcuts, const io::path_t& path) const;
    Shortcut readShortcut(deprecated::XmlReader& reader) const;

    bool writeToFile(const ShortcutList& shortcuts, const io::path_t& path) const;
    void writeShortcut(deprecated::XmlWriter& writer, const Shortcut& shortcut) const;

    void mergeShortcuts(ShortcutList& shortcuts, const ShortcutList& defaultShortcuts) const;
    void mergeAdditionalShortcuts(ShortcutList& shortcuts);

    void makeUnique(ShortcutList& shortcuts);
    void expandStandardKeys(ShortcutList& shortcuts) const;

    ShortcutList filterAndUpdateAdditionalShortcuts(const ShortcutList& shortcuts);

    ShortcutList m_shortcuts;
    ShortcutList m_defaultShortcuts;
    std::unordered_map<std::string, ShortcutList> m_additionalShortcutsMap;
    async::Notification m_shortcutsChanged;

    bool m_isActive = true;
    async::Notification m_activeChanged;
};
}

#endif // MU_SHORTCUTS_SHORTCUTSREGISTER_H
