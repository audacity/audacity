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

#ifndef MU_SHORTCUTS_SHORTCUTSMODEL_H
#define MU_SHORTCUTS_SHORTCUTSMODEL_H

#include <QAbstractListModel>
#include <QItemSelection>

#include "modularity/ioc.h"
#include "ishortcutsregister.h"
#include "ishortcutsconfiguration.h"
#include "ui/iuiactionsregister.h"
#include "async/asyncable.h"
#include "iinteractive.h"
#include "iglobalconfiguration.h"

class QItemSelection;

namespace mu::shortcuts {
class ShortcutsModel : public QAbstractListModel, public async::Asyncable
{
    Q_OBJECT

    INJECT(IShortcutsRegister, shortcutsRegister)
    INJECT(ui::IUiActionsRegister, uiactionsRegister)
    INJECT(IInteractive, interactive)
    INJECT(IShortcutsConfiguration, configuration)
    INJECT(IGlobalConfiguration, globalConfiguration)

    Q_PROPERTY(QItemSelection selection READ selection WRITE setSelection NOTIFY selectionChanged)
    Q_PROPERTY(QVariant currentShortcut READ currentShortcut NOTIFY selectionChanged)

public:
    explicit ShortcutsModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    QItemSelection selection() const;
    QVariant currentShortcut() const;

    Q_INVOKABLE void load();
    Q_INVOKABLE bool apply();
    Q_INVOKABLE void reset();

    Q_INVOKABLE void importShortcutsFromFile();
    Q_INVOKABLE void exportShortcutsToFile();

    Q_INVOKABLE void applySequenceToCurrentShortcut(const QString& newSequence, int conflictShortcutIndex = -1);

    Q_INVOKABLE void clearSelectedShortcuts();
    Q_INVOKABLE void resetToDefaultSelectedShortcuts();

    Q_INVOKABLE QVariantList shortcuts() const;

public slots:
    void setSelection(const QItemSelection& selection);

signals:
    void selectionChanged();

private:
    const ui::UiAction& action(const std::string& actionCode) const;
    QString actionText(const std::string& actionCode) const;

    QModelIndex currentShortcutIndex() const;
    void notifyAboutShortcutChanged(const QModelIndex& index);

    QVariant shortcutToObject(const Shortcut& shortcut) const;

    enum Roles {
        RoleTitle = Qt::UserRole + 1,
        RoleIcon,
        RoleSequence,
        RoleSearchKey
    };

    QList<Shortcut> m_shortcuts;
    QItemSelection m_selection;
};
}

#endif // MU_SHORTCUTS_SHORTCUTSMODEL_H
