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
#ifndef MU_SHORTCUTS_MIDIREMOTE_H
#define MU_SHORTCUTS_MIDIREMOTE_H

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "io/ifilesystem.h"
#include "actions/iactionsdispatcher.h"
#include "multiinstances/imultiinstancesprovider.h"
#include "ishortcutsconfiguration.h"

#include "shortcutstypes.h"
#include "../imidiremote.h"

namespace mu::deprecated {
class XmlReader;
class XmlWriter;
}

namespace mu::shortcuts {
class MidiRemote : public IMidiRemote, public async::Asyncable
{
    INJECT(io::IFileSystem, fileSystem)
    INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    INJECT(actions::IActionsDispatcher, dispatcher)
    INJECT(IShortcutsConfiguration, configuration)

public:
    MidiRemote() = default;

    void init();

    const MidiMappingList& midiMappings() const override;
    Ret setMidiMappings(const MidiMappingList& midiMappings) override;
    void resetMidiMappings() override;
    async::Notification midiMappingsChanged() const override;

    // Setting
    void setIsSettingMode(bool arg) override;
    bool isSettingMode() const override;

    void setCurrentActionEvent(const midi::Event& ev) override;

    // Process
    Ret process(const midi::Event& ev) override;

private:
    void readMidiMappings();
    MidiControlsMapping readMidiMapping(deprecated::XmlReader& reader) const;

    bool writeMidiMappings(const MidiMappingList& midiMappings) const;
    void writeMidiMapping(deprecated::XmlWriter& writer, const MidiControlsMapping& midiMapping) const;

    bool needIgnoreEvent(const midi::Event& event) const;

    RemoteEvent remoteEvent(const std::string& action) const;

    bool m_isSettingMode = false;

    MidiMappingList m_midiMappings;
    async::Notification m_midiMappingsChanged;
};
}

#endif // MU_SHORTCUTS_MIDIREMOTE_H
