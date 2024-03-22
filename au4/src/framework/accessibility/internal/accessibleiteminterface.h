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
#ifndef MU_ACCESSIBILITY_ACCESSIBLEITEMINTERFACE_H
#define MU_ACCESSIBILITY_ACCESSIBLEITEMINTERFACE_H

#include <QAccessibleInterface>

#include "accessibleobject.h"

#include "modularity/ioc.h"
#include "ui/iinteractiveprovider.h"

namespace mu::accessibility {
class AccessibleItemInterface : public QAccessibleInterface, public QAccessibleValueInterface, public QAccessibleTextInterface,
    public QAccessibleTableCellInterface
{
    Inject<ui::IInteractiveProvider> interactiveProvider;

public:
    AccessibleItemInterface(AccessibleObject* object);

    bool isValid() const override;
    QObject* object() const override;
    QWindow* window() const override;
    QRect rect() const override;

    QAccessibleInterface* focusChild() const override;
    QAccessibleInterface* childAt(int x, int y) const override;

    QAccessibleInterface* parent() const override;
    QAccessibleInterface* child(int index) const override;
    int childCount() const override;
    int indexOfChild(const QAccessibleInterface* iface) const override;

    QAccessible::State state() const override;
    QAccessible::Role role() const override;
    QString text(QAccessible::Text) const override;
    void setText(QAccessible::Text, const QString& text) override;

    // Value Interface
    QVariant currentValue() const override;
    void setCurrentValue(const QVariant& value) override;
    QVariant maximumValue() const override;
    QVariant minimumValue() const override;
    QVariant minimumStepSize() const override;

    // Text Interface
    void selection(int selectionIndex, int* startOffset, int* endOffset) const override;
    int selectionCount() const override;
    void addSelection(int startOffset, int endOffset) override;
    void removeSelection(int selectionIndex) override;
    void setSelection(int selectionIndex, int startOffset, int endOffset) override;

    int cursorPosition() const override;
    void setCursorPosition(int position) override;

    QString text(int startOffset, int endOffset) const override;
    QString textBeforeOffset(int offset, QAccessible::TextBoundaryType boundaryType, int* startOffset, int* endOffset) const override;
    QString textAfterOffset(int offset, QAccessible::TextBoundaryType boundaryType, int* startOffset, int* endOffset) const override;
    QString textAtOffset(int offset, QAccessible::TextBoundaryType boundaryType, int* startOffset, int* endOffset) const override;
    int characterCount() const override;

    QRect characterRect(int offset) const override;
    int offsetAtPoint(const QPoint& point) const override;

    void scrollToSubstring(int startIndex, int endIndex) override;
    QString attributes(int /* offset */, int* startOffset, int* endOffset) const override;

    // Table cell(list view item) Interface
    bool isSelected() const override;

    QList<QAccessibleInterface*> columnHeaderCells() const override;
    QList<QAccessibleInterface*> rowHeaderCells() const override;
    int columnIndex() const override;
    int rowIndex() const override;
    int columnExtent() const override;
    int rowExtent() const override;

    QAccessibleInterface* table() const override;

protected:
    void* interface_cast(QAccessible::InterfaceType t) override;

private:

    IAccessible::TextBoundaryType muBoundaryType(QAccessible::TextBoundaryType qtBoundaryType) const;

    AccessibleObject* m_object = nullptr;
};
}

#endif // MU_ACCESSIBILITY_ACCESSIBLEITEMINTERFACE_H
