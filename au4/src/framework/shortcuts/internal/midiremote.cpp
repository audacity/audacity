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
#include "midiremote.h"

#include "global/deprecated/xmlreader.h"
#include "global/deprecated/xmlwriter.h"

#include "multiinstances/resourcelockguard.h"

#include "log.h"

using namespace mu::shortcuts;
using namespace mu::midi;

constexpr std::string_view MIDIMAPPING_TAG("MidiMapping");
constexpr std::string_view EVENT_TAG("Event");
constexpr std::string_view MAPPING_ACTION_CODE_TAG("key");
constexpr std::string_view MAPPING_EVENT_TYPE_TAG("EventType");
constexpr std::string_view MAPPING_EVENT_VALUE_TAG("EventValue");

static const std::string REALTIME_ADVANCE_ACTION_NAME("realtime-advance");

static const std::string MIDI_MAPPING_RESOURCE_NAME("MIDI_MAPPING");

void MidiRemote::init()
{
    multiInstancesProvider()->resourceChanged().onReceive(this, [this](const std::string& resourceName) {
        if (resourceName == MIDI_MAPPING_RESOURCE_NAME) {
            readMidiMappings();
        }
    });

    readMidiMappings();
}

const MidiMappingList& MidiRemote::midiMappings() const
{
    return m_midiMappings;
}

mu::Ret MidiRemote::setMidiMappings(const MidiMappingList& midiMappings)
{
    if (m_midiMappings == midiMappings) {
        return true;
    }

    bool ok = writeMidiMappings(midiMappings);

    if (ok) {
        m_midiMappings = midiMappings;
        m_midiMappingsChanged.notify();
    }

    return ok;
}

void MidiRemote::resetMidiMappings()
{
    mi::WriteResourceLockGuard resource_guard(multiInstancesProvider(), MIDI_MAPPING_RESOURCE_NAME);
    fileSystem()->remove(configuration()->midiMappingUserAppDataPath());

    m_midiMappings = {};
    m_midiMappingsChanged.notify();
}

mu::async::Notification MidiRemote::midiMappingsChanged() const
{
    return m_midiMappingsChanged;
}

void MidiRemote::setIsSettingMode(bool arg)
{
    m_isSettingMode = arg;
}

bool MidiRemote::isSettingMode() const
{
    return m_isSettingMode;
}

void MidiRemote::setCurrentActionEvent(const Event& ev)
{
    UNUSED(ev);
    NOT_IMPLEMENTED;
}

mu::Ret MidiRemote::process(const Event& ev)
{
    if (needIgnoreEvent(ev)) {
        return Ret(Ret::Code::Undefined);
    }

    RemoteEvent event = remoteEventFromMidiEvent(ev);

    for (const MidiControlsMapping& midiMapping : m_midiMappings) {
        if (midiMapping.event == event) {
            dispatcher()->dispatch(midiMapping.action);
            return make_ret(Ret::Code::Ok);
        }
    }

    return Ret(Ret::Code::Undefined);
}

void MidiRemote::readMidiMappings()
{
    mi::ReadResourceLockGuard resource_guard(multiInstancesProvider(), MIDI_MAPPING_RESOURCE_NAME);

    io::path_t midiMappingsPath = configuration()->midiMappingUserAppDataPath();
    deprecated::XmlReader reader(midiMappingsPath);

    reader.readNextStartElement();
    if (reader.tagName() != MIDIMAPPING_TAG) {
        return;
    }

    while (reader.readNextStartElement()) {
        if (reader.tagName() != EVENT_TAG) {
            reader.skipCurrentElement();
            continue;
        }

        MidiControlsMapping midiMapping = readMidiMapping(reader);
        if (midiMapping.isValid()) {
            m_midiMappings.push_back(midiMapping);
        }
    }

    if (!reader.success()) {
        LOGE() << "failed parse xml, error: " << reader.error();
    }
}

MidiControlsMapping MidiRemote::readMidiMapping(deprecated::XmlReader& reader) const
{
    MidiControlsMapping midiMapping;

    while (reader.readNextStartElement()) {
        std::string tag(reader.tagName());

        if (tag == MAPPING_ACTION_CODE_TAG) {
            midiMapping.action = reader.readString();
        } else if (tag == MAPPING_EVENT_TYPE_TAG) {
            midiMapping.event.type = static_cast<RemoteEventType>(reader.readInt());
        } else if (tag == MAPPING_EVENT_VALUE_TAG) {
            midiMapping.event.value = reader.readInt();
        } else {
            reader.skipCurrentElement();
        }
    }

    return midiMapping;
}

bool MidiRemote::writeMidiMappings(const MidiMappingList& midiMappings) const
{
    TRACEFUNC;

    mi::WriteResourceLockGuard resource_guard(multiInstancesProvider(), MIDI_MAPPING_RESOURCE_NAME);

    io::path_t midiMappingsPath = configuration()->midiMappingUserAppDataPath();
    deprecated::XmlWriter writer(midiMappingsPath);

    writer.writeStartDocument();
    writer.writeStartElement(MIDIMAPPING_TAG);

    for (const MidiControlsMapping& midiMapping : midiMappings) {
        writeMidiMapping(writer, midiMapping);
    }

    writer.writeEndElement();
    writer.writeEndDocument();

    return writer.success();
}

void MidiRemote::writeMidiMapping(deprecated::XmlWriter& writer, const MidiControlsMapping& midiMapping) const
{
    writer.writeStartElement(EVENT_TAG);
    writer.writeTextElement(MAPPING_ACTION_CODE_TAG, midiMapping.action);
    writer.writeTextElement(MAPPING_EVENT_TYPE_TAG, std::to_string(midiMapping.event.type));
    writer.writeTextElement(MAPPING_EVENT_VALUE_TAG, std::to_string(midiMapping.event.value));
    writer.writeEndElement();
}

bool MidiRemote::needIgnoreEvent(const Event& event) const
{
    if (isSettingMode()) {
        return true;
    }

    if (event.opcode() != Event::Opcode::NoteOn && event.opcode() != Event::Opcode::NoteOff
        && event.opcode() != Event::Opcode::ControlChange) {
        return true;
    }

    static const QList<Event::Opcode> releaseOps {
        Event::Opcode::NoteOff
    };

    bool release = releaseOps.contains(event.opcode());
    if (release) {
        bool advanceToNextNoteOnKeyRelease = configuration()->advanceToNextNoteOnKeyRelease();
        if (!advanceToNextNoteOnKeyRelease) {
            return true;
        }

        RemoteEvent remoteEvent = remoteEventFromMidiEvent(event);
        RemoteEvent realtimeEvent = this->remoteEvent(REALTIME_ADVANCE_ACTION_NAME);
        if (!realtimeEvent.isValid() || remoteEvent != realtimeEvent) {
            return true;
        }
    }

    return false;
}

RemoteEvent MidiRemote::remoteEvent(const std::string& action) const
{
    for (const MidiControlsMapping& midiMapping : m_midiMappings) {
        if (midiMapping.action == action) {
            return midiMapping.event;
        }
    }

    return RemoteEvent();
}
