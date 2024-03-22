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

#ifndef MU_UI_QMLACCESSIBLE_H
#define MU_UI_QMLACCESSIBLE_H

#include <QObject>
#include <QQmlParserStatus>
#include <QQuickItem>
#include <QMap>

#include "accessibility/iaccessible.h"
#include "modularity/ioc.h"
#include "accessibility/iaccessibilitycontroller.h"

#define STATE_PROPERTY(P, S) \
    Q_PROPERTY(bool P READ P WRITE set_##P NOTIFY stateChanged FINAL) \
    bool P() const { return m_state.value(S, false); } \
    void set_##P(bool arg) { setState(S, arg); } \

namespace mu::ui {
class MUAccessible
{
    Q_GADGET
public:
    MUAccessible() = default;

    //! NOTE Please sync with accessibility::IAccessible::Role (src/framework/accessibility/iaccessible.h)
    enum Role {
        NoRole = 0,
        Application,
        Dialog,
        Panel,
        StaticText,
        EditableText,
        Button,
        CheckBox,
        RadioButton,
        ComboBox,
        List,
        ListItem,
        MenuItem,
        Range,
        Information
    };
    Q_ENUM(Role)
};

class AccessibleItem : public QObject, public QQmlParserStatus, public accessibility::IAccessible
{
    Q_OBJECT

    Q_PROPERTY(AccessibleItem * accessibleParent READ accessibleParent_property WRITE setAccessibleParent NOTIFY accessiblePrnChanged)
    Q_PROPERTY(mu::ui::MUAccessible::Role role READ role WRITE setRole NOTIFY roleChanged)
    Q_PROPERTY(QString name READ name WRITE setName NOTIFY nameChanged)
    Q_PROPERTY(QString description READ description WRITE setDescription NOTIFY descriptionChanged)

    Q_PROPERTY(QVariant value READ value WRITE setValue NOTIFY valueChanged)
    Q_PROPERTY(QVariant maximumValue READ maximumValue WRITE setMaximumValue NOTIFY maximumValueChanged)
    Q_PROPERTY(QVariant minimumValue READ minimumValue WRITE setMinimumValue NOTIFY minimumValueChanged)
    Q_PROPERTY(QVariant stepSize READ stepSize WRITE setStepSize NOTIFY stepSizeChanged)

    Q_PROPERTY(QString text READ text WRITE setText NOTIFY textChanged)
    Q_PROPERTY(QString selectedText READ selectedText WRITE setSelectedText NOTIFY selectedTextChanged)
    Q_PROPERTY(int selectionStart READ selectionStart WRITE setSelectionStart NOTIFY selectionStartChanged)
    Q_PROPERTY(int selectionEnd READ selectionEnd WRITE setSelectionEnd NOTIFY selectionEndChanged)
    Q_PROPERTY(int cursorPosition READ cursorPosition WRITE setCursorPosition NOTIFY cursorPositionChanged)

    Q_PROPERTY(int row READ row WRITE setRow NOTIFY rowChanged)

    Q_PROPERTY(bool ignored READ ignored WRITE setIgnored NOTIFY ignoredChanged)
    Q_PROPERTY(QQuickItem * visualItem READ visualItem WRITE setVisualItem NOTIFY visualItemChanged)

    Q_PROPERTY(QWindow * window READ window WRITE setWindow NOTIFY windowChanged)

    Q_INTERFACES(QQmlParserStatus)

    INJECT(accessibility::IAccessibilityController, accessibilityController)

public:
    STATE_PROPERTY(enabled, State::Enabled)
    STATE_PROPERTY(selected, State::Selected)
    STATE_PROPERTY(focused, State::Focused)
    STATE_PROPERTY(checked, State::Checked)

    AccessibleItem(QObject* parent = nullptr);
    ~AccessibleItem();

    // IAccessible
    const IAccessible* accessibleParent() const override;
    size_t accessibleChildCount() const override;
    const IAccessible* accessibleChild(size_t i) const override;
    QWindow* accessibleWindow() const override;

    IAccessible::Role accessibleRole() const override;
    QString accessibleName() const override;
    QString accessibleDescription() const override;
    bool accessibleState(State st) const override;
    QRect accessibleRect() const override;
    bool accessibleIgnored() const override;

    // Value Interface
    QVariant accessibleValue() const override;
    QVariant accessibleMaximumValue() const override;
    QVariant accessibleMinimumValue() const override;
    QVariant accessibleValueStepSize() const override;

    // Text Interface
    void accessibleSelection(int selectionIndex, int* startOffset, int* endOffset) const override;
    int accessibleSelectionCount() const override;

    int accessibleCursorPosition() const override;

    QString accessibleText(int startOffset, int endOffset) const override;
    QString accessibleTextBeforeOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const override;
    QString accessibleTextAfterOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const override;
    QString accessibleTextAtOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const override;
    int accessibleCharacterCount() const override;

    // ListView item Interface
    int accessibleRowIndex() const override;

    async::Channel<Property, Val> accessiblePropertyChanged() const override;

    void setState(State st, bool arg) override;
    async::Channel<State, bool> accessibleStateChanged() const override;
    // -----

    // QQmlParserStatus
    void classBegin() override;
    void componentComplete() override;

    AccessibleItem* accessibleParent_property() const;
    MUAccessible::Role role() const;
    QString name() const;
    QString description() const;

    QVariant value() const;
    QVariant maximumValue() const;
    QVariant minimumValue() const;
    QVariant stepSize() const;

    QString text() const;
    QString selectedText() const;
    int selectionStart() const;
    int selectionEnd() const;
    int cursorPosition() const;

    int row() const;

    bool ignored() const;
    QQuickItem* visualItem() const;

    QWindow* window() const;

public slots:
    void setAccessibleParent(mu::ui::AccessibleItem* p);
    void setRole(MUAccessible::Role role);
    void setName(QString name);
    void setDescription(QString description);
    void setValue(QVariant value);
    void setMaximumValue(QVariant maximumValue);
    void setMinimumValue(QVariant minimumValue);
    void setStepSize(QVariant stepSize);
    void setText(const QString& text);
    void setSelectedText(const QString& selectedText);
    void setSelectionStart(int selectionStart);
    void setSelectionEnd(int selectionEnd);
    void setCursorPosition(int cursorPosition);
    void setRow(int row);
    void setIgnored(bool ignored);
    void setVisualItem(QQuickItem* item);
    void setWindow(QWindow* window);

signals:
    void accessiblePrnChanged();
    void roleChanged(MUAccessible::Role role);
    void nameChanged(QString name);
    void descriptionChanged(QString description);
    void valueChanged(QVariant value);
    void maximumValueChanged(QVariant maximumValue);
    void minimumValueChanged(QVariant minimumValue);
    void stepSizeChanged(QVariant stepSize);
    void textChanged();
    void selectedTextChanged();
    void selectionStartChanged();
    void selectionEndChanged();
    void cursorPositionChanged();
    void rowChanged();
    void ignoredChanged(bool ignored);
    void visualItemChanged(QQuickItem* item);
    void stateChanged();
    void windowChanged();

private:
    const IAccessible* accessibleRoot() const;

    void addChild(AccessibleItem* item);
    void removeChild(AccessibleItem* item);

    QQuickItem* resolveVisualItem() const;

    bool m_registred = false;
    AccessibleItem* m_accessibleParent = nullptr;
    QList<AccessibleItem*> m_children;
    MUAccessible::Role m_role = MUAccessible::NoRole;
    QString m_name;
    QString m_description;
    QVariant m_value;
    QVariant m_maximumValue;
    QVariant m_minimumValue;
    QVariant m_stepSize;
    QString m_text;
    QString m_selectedText;
    int m_selectionStart = 0;
    int m_selectionEnd = 0;
    int m_cursorPosition = 0;
    int m_row = 0;
    bool m_ignored = false;
    QQuickItem* m_visualItem = nullptr;
    QWindow* m_window = nullptr;
    QMap<State, bool> m_state;
    async::Channel<Property, Val> m_accessiblePropertyChanged;
    async::Channel<State, bool> m_accessibleStateChanged;
};
}

#endif // MU_UI_QMLACCESSIBLE_H
