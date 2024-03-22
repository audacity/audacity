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

#ifndef MU_UICOMPONENTS_BUTTONBOXMODEL_H
#define MU_UICOMPONENTS_BUTTONBOXMODEL_H

#include <QObject>
#include <QQuickItem>

#include "translation.h"
#include "qmllistproperty.h"

namespace mu::uicomponents {
class ButtonBoxModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(ButtonLayout buttonLayout READ buttonLayout WRITE setButtonLayout NOTIFY buttonLayoutChanged)

    Q_PROPERTY(QQmlListProperty<QQuickItem> buttonsItems READ buttonsItems NOTIFY buttonsItemsChanged)

public:
    explicit ButtonBoxModel(QObject* parent = nullptr);

    enum ButtonType {
        NoButton,
        Ok,
        Continue,
        RestoreDefaults,
        Reset,
        Apply,
        Help,
        Discard,
        Cancel,
        Close,
        Ignore,
        Retry,
        Abort,
        NoToAll,
        No,
        YesToAll,
        Yes,
        Open,
        DontSave,
        SaveAll,
        Save,
        Next,
        Back,
        Select,
        Clear,
        Done,

        CustomButton,

        FirstButton       = Ok,
        LastButton        = CustomButton
    };
    Q_ENUM(ButtonType)

    enum ButtonRole { // Keep updated with buttonRoleLayouts
        AcceptRole,
        RejectRole,
        DestructiveRole,
        ResetRole,
        ApplyRole,
        RetryRole,
        HelpRole,
        ContinueRole,
        BackRole,
        CustomRole
    };
    Q_ENUM(ButtonRole)

    enum ButtonLayout {
        UnknownLayout = -1,
        WinLayout,
        MacLayout,
        LinuxLayout
    };
    Q_ENUM(ButtonLayout)

    Q_INVOKABLE QList<int> load();
    Q_INVOKABLE void setButtons(const QVariantList& buttons);

    ButtonLayout buttonLayout() const;
    void setButtonLayout(ButtonLayout newButtonLayout);

    QQmlListProperty<QQuickItem> buttonsItems();

signals:
    void buttonLayoutChanged();
    void buttonsItemsChanged();

    void addButton(QString text, int buttonType, ButtonRole buttonRole, bool isAccent, bool isLeftSide);
    void reloadRequested();

private:
    const std::vector<ButtonRole>& chooseButtonLayoutType();

    struct LayoutButton {
        QString text;
        int buttonType = int(ButtonType::NoButton);
        ButtonRole buttonRole = ButtonRole::CustomRole;
        bool isAccent = false;
        bool isLeftSide = false;

        LayoutButton(QString _text, int _buttonType,  ButtonRole _buttonRole, bool _isAccent, bool _isLeftSide = false)
            : text(_text), buttonType(_buttonType),  buttonRole(_buttonRole), isAccent(_isAccent), isLeftSide(_isLeftSide)
        {
        }
    };

    LayoutButton* layoutButton(const QQuickItem* item) const;

    QHash<ButtonType, LayoutButton*> m_layoutButtons {
        { Ok,              new LayoutButton(qtrc("uicomponents", "OK"),               Ok,              AcceptRole,      true) },
        { Save,            new LayoutButton(qtrc("uicomponents", "Save"),             Save,            ApplyRole,       true) },
        { SaveAll,         new LayoutButton(qtrc("uicomponents", "Save all"),         SaveAll,         ApplyRole,       false) },
        { DontSave,        new LayoutButton(qtrc("uicomponents", "Don’t save"),       DontSave,        DestructiveRole, false) },
        { Open,            new LayoutButton(qtrc("uicomponents", "Open"),             Open,            AcceptRole,      true) },
        { Yes,             new LayoutButton(qtrc("uicomponents", "Yes"),              Yes,             AcceptRole,      true) },
        { YesToAll,        new LayoutButton(qtrc("uicomponents", "Yes to all"),       YesToAll,        AcceptRole,      false) },
        { No,              new LayoutButton(qtrc("uicomponents", "No"),               No,              RejectRole,      false) },
        { NoToAll,         new LayoutButton(qtrc("uicomponents", "No to all"),        NoToAll,         RejectRole,      false) },
        { Abort,           new LayoutButton(qtrc("uicomponents", "Abort"),            Abort,           RejectRole,      false) },
        { Retry,           new LayoutButton(qtrc("uicomponents", "Retry"),            Retry,           RetryRole,       false) },
        { Ignore,          new LayoutButton(qtrc("uicomponents", "Ignore"),           Ignore,          DestructiveRole, false) },
        { Close,           new LayoutButton(qtrc("uicomponents", "Close"),            Close,           RejectRole,      false) },
        { Cancel,          new LayoutButton(qtrc("uicomponents", "Cancel"),           Cancel,          RejectRole,      false) },
        { Discard,         new LayoutButton(qtrc("uicomponents", "Discard"),          Discard,         DestructiveRole, false) },
        { Help,            new LayoutButton(qtrc("uicomponents", "Help"),             Help,            HelpRole,        false) },
        { Apply,           new LayoutButton(qtrc("uicomponents", "Apply"),            Apply,           ApplyRole,       false) },
        { Reset,           new LayoutButton(qtrc("uicomponents", "Reset"),            Reset,           ResetRole,       false) },
        { RestoreDefaults, new LayoutButton(qtrc("uicomponents", "Restore defaults"), RestoreDefaults, ResetRole,       false) },
        { Continue,        new LayoutButton(qtrc("uicomponents", "Continue"),         Continue,        ContinueRole,    true) },
        { Next,            new LayoutButton(qtrc("uicomponents", "Next"),             Next,            ContinueRole,    false) },
        { Back,            new LayoutButton(qtrc("uicomponents", "Back"),             Back,            BackRole,        false) },
        { Select,          new LayoutButton(qtrc("uicomponents", "Select"),           Select,          AcceptRole,      true) },
        { Clear,           new LayoutButton(qtrc("uicomponents", "Clear"),            Clear,           DestructiveRole, false) },
        { Done,            new LayoutButton(qtrc("uicomponents", "Done"),             Done,            AcceptRole,      true) }
    };

    std::vector<std::vector<ButtonRole> > buttonRoleLayouts = {
        // WinLayout
        std::vector <ButtonRole> { CustomRole, ResetRole, RetryRole, BackRole, AcceptRole, ApplyRole, ContinueRole, DestructiveRole,
                                   RejectRole, HelpRole },

        // MacLayout
        std::vector <ButtonRole> { CustomRole, HelpRole, ResetRole, RetryRole, DestructiveRole, RejectRole, BackRole, AcceptRole,
                                   ApplyRole, ContinueRole },

        // LinuxLayout
        std::vector <ButtonRole> { CustomRole, HelpRole, ResetRole, RetryRole, DestructiveRole, RejectRole, BackRole, AcceptRole,
                                   ApplyRole, ContinueRole }
    };

    ButtonLayout m_buttonLayout = ButtonLayout::UnknownLayout;
    QmlListProperty<QQuickItem> m_buttonsItems;
};
}

#endif // MU_UICOMPONENTS_BUTTONBOXMODEL_H
