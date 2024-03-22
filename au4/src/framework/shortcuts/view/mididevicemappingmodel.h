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

#ifndef MU_SHORTCUTS_MIDIDEVICEMAPPINGMODEL_H
#define MU_SHORTCUTS_MIDIDEVICEMAPPINGMODEL_H

#include <QAbstractListModel>
#include <QItemSelection>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "midi/miditypes.h"
#include "midi/imidiconfiguration.h"
#include "ishortcutsconfiguration.h"
#include "imidiremote.h"

#include "ui/iuiactionsregister.h"
#include "ui/uitypes.h"

namespace mu::shortcuts {
class MidiDeviceMappingModel : public QAbstractListModel, public async::Asyncable
{
    Q_OBJECT

    INJECT(ui::IUiActionsRegister, uiActionsRegister)
    INJECT(shortcuts::IMidiRemote, midiRemote)
    INJECT(IShortcutsConfiguration, configuration)
    INJECT(midi::IMidiConfiguration, midiConfiguration)

    Q_PROPERTY(bool useRemoteControl READ useRemoteControl WRITE setUseRemoteControl NOTIFY useRemoteControlChanged)

    Q_PROPERTY(QItemSelection selection READ selection WRITE setSelection NOTIFY selectionChanged)
    Q_PROPERTY(bool canEditAction READ canEditAction NOTIFY selectionChanged)

public:
    explicit MidiDeviceMappingModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool useRemoteControl() const;
    QItemSelection selection() const;
    bool canEditAction() const;

    Q_INVOKABLE void load();
    Q_INVOKABLE bool apply();
    Q_INVOKABLE void reset();

    Q_INVOKABLE void clearSelectedActions();
    Q_INVOKABLE void clearAllActions();

    Q_INVOKABLE QVariant currentAction() const;
    Q_INVOKABLE void mapCurrentActionToMidiEvent(const QVariant& event);

public slots:
    void setUseRemoteControl(bool value);
    void setSelection(const QItemSelection& selection);

signals:
    void useRemoteControlChanged(bool value);
    void selectionChanged(const QItemSelection& selection);

private:
    enum Roles {
        RoleTitle = Qt::UserRole + 1,
        RoleIcon,
        RoleEnabled,
        RoleStatus,
        RoleMappedType,
        RoleMappedValue
    };

    QVariantMap midiMappingToObject(const MidiControlsMapping& midiMapping) const;

    QList<MidiControlsMapping> m_midiMappings;
    QItemSelection m_selection;
};
}

#endif // MU_SHORTCUTS_MIDIDEVICEMAPPINGMODEL_H
