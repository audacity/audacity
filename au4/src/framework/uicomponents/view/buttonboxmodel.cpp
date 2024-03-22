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

#include "buttonboxmodel.h"

#include "log.h"

using namespace mu::uicomponents;

ButtonBoxModel::ButtonBoxModel(QObject* parent)
    : QObject(parent), m_buttonsItems(this)
{
}

QList<int> ButtonBoxModel::load()
{
    QList<int> result;
    std::unordered_map <int, std::vector <LayoutButton*> > sortedButtons;
    int maxCustomRole = static_cast<int>(ButtonRole::CustomRole);

    QList<QQuickItem*> buttonsItems = m_buttonsItems.list();
    for (const QQuickItem* item : buttonsItems) {
        QVariant buttonTypeVar = item->property("buttonId");
        int type = static_cast<int>(ButtonType::CustomButton);
        LayoutButton* button = nullptr;
        if (buttonTypeVar.isValid()) {
            type = buttonTypeVar.toInt();
            if (type >= static_cast<int>(ButtonType::CustomButton)) {
                button = layoutButton(item);
            } else {
                button = m_layoutButtons[static_cast<ButtonType>(type)];
            }
        } else {
            button = m_layoutButtons[static_cast<ButtonType>(type)];
        }

        ButtonRole role = button->buttonRole;
        if (role >= ButtonRole::CustomRole) {
            maxCustomRole = std::max(maxCustomRole, static_cast<int>(role));
        }

        sortedButtons[role].push_back(button);
    }

    const std::vector<ButtonRole>& currentLayout = chooseButtonLayoutType();

    auto buttonsByRole = [&sortedButtons, maxCustomRole](ButtonRole buttonRole) -> std::vector<LayoutButton*> {
        bool isCustom = buttonRole == ButtonRole::CustomRole;
        if (!isCustom) {
            if (contains(sortedButtons, static_cast<int>(buttonRole))) {
                return sortedButtons[buttonRole];
            }

            return {};
        }

        std::vector<LayoutButton*> buttons;
        for (int role = static_cast<int>(ButtonRole::CustomRole); role <= maxCustomRole; ++role) {
            for (LayoutButton* button : sortedButtons[role]) {
                buttons.push_back(button);
            }
        }

        return buttons;
    };

    for (ButtonRole buttonRole : currentLayout) {
        std::vector<LayoutButton*> buttons = buttonsByRole(buttonRole);

        for (LayoutButton* button : buttons) {
            result << button->buttonType;
        }
    }

    return result;
}

void ButtonBoxModel::setButtons(const QVariantList& buttons)
{
    for (const QVariant& buttonTypeVar : buttons) {
        LayoutButton* button = m_layoutButtons[ButtonType(buttonTypeVar.toInt())];
        emit addButton(button->text, button->buttonType, button->buttonRole, button->isAccent, button->isLeftSide);
    }
}

const std::vector <ButtonBoxModel::ButtonRole>& ButtonBoxModel::chooseButtonLayoutType()
{
    size_t index = 0;
    if (m_buttonLayout != ButtonLayout::UnknownLayout) {
        index = static_cast<size_t>(m_buttonLayout);
    } else {
#if defined (Q_OS_OSX)
        index = 1;
#elif defined (Q_OS_LINUX) || defined (Q_OS_UNIX) || defined(Q_OS_FREEBSD)
        index = 2;
#endif
    }

    IF_ASSERT_FAILED(index < buttonRoleLayouts.size()) {
        index = 0;
    }

    return buttonRoleLayouts[index];
}

ButtonBoxModel::LayoutButton* ButtonBoxModel::layoutButton(const QQuickItem* item) const
{
    QString text = item->property("text").toString();
    ButtonRole role = static_cast<ButtonRole>(item->property("buttonRole").toInt());
    int type = item->property("buttonId").toInt();
    bool isAccent = item->property("accentButton").toBool();
    bool isLeftSide = item->property("isLeftSide").toBool();

    return new LayoutButton(text, type, role, isAccent, isLeftSide);
}

ButtonBoxModel::ButtonLayout ButtonBoxModel::buttonLayout() const
{
    return m_buttonLayout;
}

void ButtonBoxModel::setButtonLayout(ButtonLayout newButtonLayout)
{
    if (m_buttonLayout == newButtonLayout) {
        return;
    }
    m_buttonLayout = newButtonLayout;
    emit buttonLayoutChanged();

    emit reloadRequested();
}

QQmlListProperty<QQuickItem> ButtonBoxModel::buttonsItems()
{
    return m_buttonsItems.property();
}
