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
#include "menuitem.h"

#include <QVariantMap>

#include "types/translatablestring.h"
#include "shortcuts/shortcutstypes.h"

using namespace mu::uicomponents;
using namespace mu::ui;

MenuItem::MenuItem(QObject* parent)
    : QObject(parent)
{
}

MenuItem::MenuItem(const UiAction& action, QObject* parent)
    : QObject(parent)
{
    m_id = QString::fromStdString(action.code);
    m_action = action;
}

QString MenuItem::id() const
{
    return m_id;
}

QString MenuItem::translatedTitle() const
{
    return m_action.title.qTranslatedWithoutMnemonic();
}

QString MenuItem::titleWithMnemonicUnderline() const
{
    return m_action.title.qTranslatedWithMnemonicUnderline();
}

QString MenuItem::section() const
{
    return m_section;
}

UiActionState MenuItem::state() const
{
    return m_state;
}

bool MenuItem::selectable() const
{
    return m_selectable;
}

bool MenuItem::selected() const
{
    return m_selected;
}

MenuItemRole MenuItem::role() const
{
    return m_role;
}

QList<MenuItem*> MenuItem::subitems() const
{
    return m_subitems;
}

UiAction MenuItem::action() const
{
    return m_action;
}

mu::actions::ActionData MenuItem::args() const
{
    return m_args;
}

bool MenuItem::isValid() const
{
    return !m_id.isEmpty();
}

QString MenuItem::shortcutsTitle() const
{
    return mu::shortcuts::sequencesToNativeText(m_action.shortcuts);
}

QString MenuItem::portableShortcuts() const
{
    return QString::fromStdString(mu::shortcuts::Shortcut::sequencesToString(m_action.shortcuts));
}

void MenuItem::setId(const QString& id)
{
    if (m_id == id) {
        return;
    }

    m_id = id;
    emit idChanged(m_id);
}

void MenuItem::setTitle(const TranslatableString& title)
{
    if (m_action.title == title) {
        return;
    }

    m_action.title = title;
    emit actionChanged();
}

void MenuItem::setSection(const QString& section)
{
    if (m_section == section) {
        return;
    }

    m_section = section;
    emit sectionChanged(m_section);
}

void MenuItem::setState(const UiActionState& state)
{
    if (m_state == state) {
        return;
    }

    m_state = state;
    emit stateChanged();
}

void MenuItem::setSelectable(bool selectable)
{
    if (m_selectable == selectable) {
        return;
    }

    m_selectable = selectable;
    emit selectableChanged(m_selectable);
}

void MenuItem::setSelected(bool selected)
{
    if (m_selected == selected) {
        return;
    }

    m_selected = selected;
    emit selectedChanged(m_selected);
}

void MenuItem::setRole(MenuItemRole role)
{
    if (m_role == role) {
        return;
    }

    m_role = role;
    emit roleChanged(role_property());
}

void MenuItem::setSubitems(const QList<MenuItem*>& subitems)
{
    if (m_subitems == subitems) {
        return;
    }

    m_subitems = subitems;
    emit subitemsChanged(m_subitems, m_id);
}

void MenuItem::setAction(const UiAction& action)
{
    if (m_action == action) {
        return;
    }

    m_action = action;
    emit actionChanged();
}

void MenuItem::setArgs(const mu::actions::ActionData& args)
{
    m_args = args;
}

QString mu::uicomponents::MenuItem::code_property() const
{
    return QString::fromStdString(m_action.code);
}

QString MenuItem::description_property() const
{
    return m_action.description.qTranslated();
}

int MenuItem::icon_property() const
{
    return static_cast<int>(m_action.iconCode);
}

bool MenuItem::enabled_property() const
{
    return m_state.enabled;
}

bool MenuItem::checkable_property() const
{
    return m_action.checkable == Checkable::Yes;
}

bool MenuItem::checked_property() const
{
    return m_state.checked;
}

bool MenuItem::selectable_property() const
{
    return m_selectable;
}

bool MenuItem::selected_property() const
{
    return m_selected;
}

int MenuItem::role_property() const
{
    return static_cast<int>(m_role);
}
