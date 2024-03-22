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
#include "shortcutsinstancemodel.h"

#include "log.h"

using namespace mu::shortcuts;

ShortcutsInstanceModel::ShortcutsInstanceModel(QObject* parent)
    : QObject(parent)
{
}

void ShortcutsInstanceModel::init()
{
    shortcutsRegister()->shortcutsChanged().onNotify(this, [this](){
        doLoadShortcuts();
    });

    shortcutsRegister()->activeChanged().onNotify(this, [this](){
        emit activeChanged();
    });

    doLoadShortcuts();
}

QStringList ShortcutsInstanceModel::shortcuts() const
{
    return m_shortcuts;
}

bool ShortcutsInstanceModel::active() const
{
    return shortcutsRegister()->active();
}

void ShortcutsInstanceModel::activate(const QString& key)
{
    doActivate(key);
}

void ShortcutsInstanceModel::doLoadShortcuts()
{
    m_shortcuts.clear();

    const ShortcutList& shortcuts = shortcutsRegister()->shortcuts();
    for (const Shortcut& sc : shortcuts) {
        for (const std::string& seq : sc.sequences) {
            QString sequence = QString::fromStdString(seq);

            //! NOTE There may be several identical shortcuts for different contexts.
            //! We only need a list of unique ones.
            if (!m_shortcuts.contains(sequence)) {
                m_shortcuts << sequence;
            }
        }
    }

    emit shortcutsChanged();
}

void ShortcutsInstanceModel::doActivate(const QString& key)
{
    controller()->activate(key.toStdString());
}
