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
#ifndef MU_UICOMPONENTS_TEXTINPUTFIELDMODEL_H
#define MU_UICOMPONENTS_TEXTINPUTFIELDMODEL_H

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "shortcuts/ishortcutsregister.h"
#include "actions/iactionsdispatcher.h"

namespace mu::uicomponents {
class TextInputFieldModel : public QObject, public async::Asyncable
{
    Q_OBJECT

    INJECT(shortcuts::IShortcutsRegister, shortcutsRegister)
    INJECT(actions::IActionsDispatcher, dispatcher)

public:
    explicit TextInputFieldModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE bool isShortcutAllowedOverride(Qt::Key key, Qt::KeyboardModifiers modifiers) const;
    Q_INVOKABLE bool handleShortcut(Qt::Key key, Qt::KeyboardModifiers modifiers);

private:
    void loadShortcuts();
    shortcuts::Shortcut shortcut(Qt::Key key, Qt::KeyboardModifiers modifiers) const;

    shortcuts::ShortcutList m_notAllowedForOverrideShortcuts;
};
}

#endif // MU_UICOMPONENTS_TEXTINPUTFIELDMODEL_H
