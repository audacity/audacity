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

#include "mididevicemappingmodel.h"

#include "ui/view/iconcodes.h"
#include "shortcuts/shortcutstypes.h"

#include "log.h"
#include "translation.h"

using namespace mu::shortcuts;
using namespace mu::midi;
using namespace mu::ui;
using namespace mu::actions;

static const QString TITLE_KEY("title");
static const QString ICON_KEY("icon");
static const QString STATUS_KEY("status");
static const QString ENABLED_KEY("enabled");
static const QString MAPPED_TYPE_KEY("mappedType");
static const QString MAPPED_VALUE_KEY("mappedValue");

inline ActionCodeList allMidiActions()
{
    return {
        "rewind",
        "loop",
        "play",
        "stop",
        "note-input",
        "pad-note-1",
        "pad-note-2",
        "pad-note-4",
        "pad-note-8",
        "pad-note-16",
        "pad-note-32",
        "pad-note-64",
        "undo",
        "pad-rest",
        "tie",
        "pad-dot",
        "pad-dot2",
        "realtime-advance"
    };
}

MidiDeviceMappingModel::MidiDeviceMappingModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

QVariant MidiDeviceMappingModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    QVariantMap obj = midiMappingToObject(m_midiMappings[index.row()]);

    switch (role) {
    case RoleTitle: return obj[TITLE_KEY].toString();
    case RoleIcon: return obj[ICON_KEY].toInt();
    case RoleStatus: return obj[STATUS_KEY].toString();
    case RoleEnabled: return obj[ENABLED_KEY].toBool();
    case RoleMappedType: return obj[MAPPED_TYPE_KEY].toInt();
    case RoleMappedValue: return obj[MAPPED_VALUE_KEY].toInt();
    }

    return QVariant();
}

QVariantMap MidiDeviceMappingModel::midiMappingToObject(const MidiControlsMapping& midiMapping) const
{
    UiAction action = uiActionsRegister()->action(midiMapping.action);

    QVariantMap obj;

    obj[TITLE_KEY] = !action.description.isEmpty() ? action.description.qTranslated() : action.title.qTranslatedWithoutMnemonic();
    obj[ICON_KEY] = static_cast<int>(action.iconCode);
    obj[ENABLED_KEY] = midiMapping.isValid();
    obj[STATUS_KEY] = midiMapping.isValid() ? midiMapping.event.name().toQString() : qtrc("shortcuts", "Inactive");
    obj[MAPPED_TYPE_KEY] = static_cast<int>(midiMapping.event.type);
    obj[MAPPED_VALUE_KEY] = midiMapping.event.value;

    return obj;
}

int MidiDeviceMappingModel::rowCount(const QModelIndex&) const
{
    return m_midiMappings.size();
}

QHash<int, QByteArray> MidiDeviceMappingModel::roleNames() const
{
    return {
        { RoleTitle, TITLE_KEY.toUtf8() },
        { RoleIcon, ICON_KEY.toUtf8() },
        { RoleStatus, STATUS_KEY.toUtf8() },
        { RoleEnabled, ENABLED_KEY.toUtf8() },
        { RoleMappedType, MAPPED_TYPE_KEY.toUtf8() },
        { RoleMappedValue, MAPPED_VALUE_KEY.toUtf8() }
    };
}

void MidiDeviceMappingModel::load()
{
    beginResetModel();
    m_midiMappings.clear();

    shortcuts::MidiMappingList midiMappings = midiRemote()->midiMappings();

    auto remoteEvent = [&midiMappings](const ActionCode& actionCode) {
        for (const MidiControlsMapping& midiMapping : midiMappings) {
            if (midiMapping.action == actionCode) {
                return midiMapping.event;
            }
        }

        return RemoteEvent();
    };

    for (const ActionCode& actionCode : allMidiActions()) {
        UiAction action = uiActionsRegister()->action(actionCode);

        if (action.isValid()) {
            MidiControlsMapping midiMapping(actionCode);
            midiMapping.event = remoteEvent(actionCode);
            m_midiMappings.push_back(midiMapping);
        }
    }

    midiRemote()->midiMappingsChanged().onNotify(this, [this](){
        load();
    });

    endResetModel();
}

bool MidiDeviceMappingModel::apply()
{
    MidiMappingList midiMappings;
    for (const MidiControlsMapping& midiMapping : m_midiMappings) {
        midiMappings.push_back(midiMapping);
    }

    Ret ret = midiRemote()->setMidiMappings(midiMappings);
    if (!ret) {
        LOGE() << ret.toString();
    }

    return ret;
}

void MidiDeviceMappingModel::reset()
{
    midiRemote()->resetMidiMappings();
}

bool MidiDeviceMappingModel::useRemoteControl() const
{
    return midiConfiguration()->useRemoteControl();
}

void MidiDeviceMappingModel::setUseRemoteControl(bool value)
{
    if (value == useRemoteControl()) {
        return;
    }

    midiConfiguration()->setUseRemoteControl(value);
    emit useRemoteControlChanged(value);
}

QItemSelection MidiDeviceMappingModel::selection() const
{
    return m_selection;
}

bool MidiDeviceMappingModel::canEditAction() const
{
    return currentAction().isValid();
}

void MidiDeviceMappingModel::setSelection(const QItemSelection& selection)
{
    if (selection == m_selection) {
        return;
    }

    m_selection = selection;
    emit selectionChanged(selection);
}

void MidiDeviceMappingModel::clearSelectedActions()
{
    for (const QModelIndex& index : m_selection.indexes()) {
        m_midiMappings[index.row()].event = RemoteEvent();
        emit dataChanged(index, index);
    }
}

void MidiDeviceMappingModel::clearAllActions()
{
    beginResetModel();

    for (MidiControlsMapping& midiMapping: m_midiMappings) {
        midiMapping.event = RemoteEvent();
    }

    endResetModel();
}

QVariant MidiDeviceMappingModel::currentAction() const
{
    QModelIndexList indexes = m_selection.indexes();
    if (indexes.empty()) {
        return QVariant();
    }

    MidiControlsMapping midiMapping = m_midiMappings[indexes.first().row()];
    return midiMappingToObject(midiMapping);
}

void MidiDeviceMappingModel::mapCurrentActionToMidiEvent(const QVariant& event)
{
    QModelIndexList indexes = m_selection.indexes();
    if (indexes.empty()) {
        return;
    }

    QVariantMap eventMap = event.toMap();
    RemoteEventType type = static_cast<RemoteEventType>(eventMap["type"].toInt());
    int value = eventMap["value"].toInt();

    QModelIndex first = indexes.first();

    m_midiMappings[first.row()].event = RemoteEvent(type, value);
    emit dataChanged(first, first);
}
