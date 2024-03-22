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
#ifndef MU_SHORTCUTS_IMIDIREMOTE_H
#define MU_SHORTCUTS_IMIDIREMOTE_H

#include "modularity/imoduleinterface.h"
#include "midi/miditypes.h"
#include "types/ret.h"
#include "shortcutstypes.h"

namespace mu::shortcuts {
class IMidiRemote : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IMidiRemote)
public:
    virtual ~IMidiRemote() = default;

    virtual const MidiMappingList& midiMappings() const = 0;
    virtual Ret setMidiMappings(const MidiMappingList& midiMappings) = 0;
    virtual void resetMidiMappings() = 0;
    virtual async::Notification midiMappingsChanged() const = 0;

    // Setting
    virtual void setIsSettingMode(bool arg) = 0;
    virtual bool isSettingMode() const = 0;

    virtual void setCurrentActionEvent(const midi::Event& ev) = 0;

    // Process
    virtual Ret process(const midi::Event& ev) = 0;
};
}

#endif // MU_SHORTCUTS_IMIDIREMOTE_H
