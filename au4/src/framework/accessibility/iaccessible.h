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
#ifndef MU_ACCESSIBILITY_IACCESSIBLE_H
#define MU_ACCESSIBILITY_IACCESSIBLE_H

#include <QString>
#include <QRect>
#include <QVariant>
#include <QMap>

#include "global/async/channel.h"
#include "global/types/val.h"

class QWindow;

namespace mu::accessibility {
class IAccessible
{
public:

    virtual ~IAccessible() = default;

    //! NOTE Please sync with ui::MUAccessible::Role (src/framework/ui/view/qmlaccessible.h)
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
        Group,

        // Custom roles
        Information, // just text

        // Score roles
        ElementOnScore
    };

    enum class State {
        Undefined = 0,
        Enabled,
        Active,
        Focused,
        Selected,
        Checked
    };

    enum class Property {
        Undefined = 0,
        Parent,
        Name,
        Description,
        Value,
        TextCursor,
        TextInsert,
        TextRemove
    };

    enum TextBoundaryType {
        CharBoundary,
        WordBoundary,
        SentenceBoundary,
        ParagraphBoundary,
        LineBoundary,
        NoBoundary
    };

    struct TextRange {
        int startPosition = 0;
        int endPosition = 0;
        QString text;

        TextRange(int startPosition, int endPosition, const QString& text)
            : startPosition(startPosition), endPosition(endPosition), text(text) {}
        TextRange(const QVariantMap& map)
        {
            startPosition = map.value("startPosition").toInt();
            endPosition = map.value("endPosition").toInt();
            text = map.value("startPosition").toString();
        }

        QVariantMap toMap() const
        {
            return {
                { "startPosition", startPosition },
                { "endPosition", endPosition },
                { "text", text }
            };
        }
    };

    virtual const IAccessible* accessibleParent() const = 0;
    virtual size_t accessibleChildCount() const = 0;
    virtual const IAccessible* accessibleChild(size_t i) const = 0;
    virtual QWindow* accessibleWindow() const = 0;

    virtual IAccessible::Role accessibleRole() const = 0;
    virtual QString accessibleName() const = 0;
    virtual QString accessibleDescription() const = 0;
    virtual bool accessibleState(State st) const = 0;
    virtual QRect accessibleRect() const = 0;
    virtual bool accessibleIgnored() const = 0;

    // Value Interface
    virtual QVariant accessibleValue() const = 0;
    virtual QVariant accessibleMaximumValue() const = 0;
    virtual QVariant accessibleMinimumValue() const = 0;
    virtual QVariant accessibleValueStepSize() const = 0;

    // Text Interface
    virtual void accessibleSelection(int selectionIndex, int* startOffset, int* endOffset) const = 0;
    virtual int accessibleSelectionCount() const = 0;

    virtual int accessibleCursorPosition() const = 0;

    virtual QString accessibleText(int startOffset, int endOffset) const = 0;
    virtual QString accessibleTextBeforeOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const = 0;
    virtual QString accessibleTextAfterOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const = 0;
    virtual QString accessibleTextAtOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const = 0;
    virtual int accessibleCharacterCount() const = 0;

    // ListView item Interface
    virtual int accessibleRowIndex() const = 0;

    virtual async::Channel<IAccessible::Property, Val> accessiblePropertyChanged() const = 0;

    virtual void setState(State state, bool arg) = 0;
    virtual async::Channel<IAccessible::State, bool> accessibleStateChanged() const = 0;
};
}

#endif // MU_ACCESSIBILITY_IACCESSIBLE_H
