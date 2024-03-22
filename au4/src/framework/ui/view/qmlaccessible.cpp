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

#include "qmlaccessible.h"

#include <QQuickWindow>
#include <QTextBoundaryFinder>

#include "log.h"

using namespace mu::ui;
using namespace mu::accessibility;

AccessibleItem::AccessibleItem(QObject* parent)
    : QObject(parent)
{
}

AccessibleItem::~AccessibleItem()
{
    if (m_accessibleParent) {
        m_accessibleParent->removeChild(this);
    }

    QList<AccessibleItem*> children = m_children;
    for (AccessibleItem* ch : children) {
        ch->setAccessibleParent(nullptr);
    }

    if (m_registred) {
        accessibilityController()->unreg(this);
        m_registred = false;
    }
}

const IAccessible* AccessibleItem::accessibleParent() const
{
    if (m_accessibleParent) {
        return static_cast<const IAccessible*>(m_accessibleParent);
    }

    return accessibleRoot();
}

IAccessible::Role AccessibleItem::accessibleRole() const
{
    return static_cast<IAccessible::Role>(m_role);
}

QString AccessibleItem::accessibleName() const
{
    return m_name;
}

QString AccessibleItem::accessibleDescription() const
{
    return m_description;
}

bool AccessibleItem::accessibleState(State st) const
{
    return m_state.value(st, false);
}

size_t AccessibleItem::accessibleChildCount() const
{
    return static_cast<size_t>(m_children.size());
}

const IAccessible* AccessibleItem::accessibleChild(size_t i) const
{
    return static_cast<const IAccessible*>(m_children.value(static_cast<int>(i), nullptr));
}

QWindow* AccessibleItem::accessibleWindow() const
{
    if (m_window) {
        return m_window;
    }

    QQuickItem* visualItem = resolveVisualItem();
    if (!visualItem) {
        return nullptr;
    }

    return visualItem->window();
}

QRect AccessibleItem::accessibleRect() const
{
    QQuickItem* vitem = resolveVisualItem();
    if (!vitem || !vitem->window()) {
        return QRect();
    }

    QPointF scenePos = vitem->mapToScene(QPointF(0, 0));
    QPoint globalPos = vitem->window()->mapToGlobal(scenePos.toPoint());
    return QRect(globalPos.x(), globalPos.y(), vitem->width(), vitem->height());
}

bool AccessibleItem::accessibleIgnored() const
{
    return m_ignored;
}

QVariant AccessibleItem::accessibleValue() const
{
    return m_value;
}

QVariant AccessibleItem::accessibleMaximumValue() const
{
    return m_maximumValue;
}

QVariant AccessibleItem::accessibleMinimumValue() const
{
    return m_minimumValue;
}

QVariant AccessibleItem::accessibleValueStepSize() const
{
    return m_stepSize;
}

void AccessibleItem::accessibleSelection(int selectionIndex, int* startOffset, int* endOffset) const
{
    if (selectionIndex == 0) {
        *startOffset = m_selectionStart;
        *endOffset = m_selectionEnd;
    } else {
        *startOffset = 0;
        *endOffset = 0;
    }
}

int AccessibleItem::accessibleSelectionCount() const
{
    return m_selectedText.size();
}

int AccessibleItem::accessibleCursorPosition() const
{
    return cursorPosition();
}

QString AccessibleItem::accessibleText(int start, int end) const
{
#if defined(Q_OS_LINUX)
    return text().mid(start, end - start + 1);
#else
    return text().mid(start, end - start);
#endif
}

//! NOTE: qaccessible.cpp - textLineBoundary
static QString textLineBoundary(int beforeAtAfter, const QString& text, int offset, int* startOffset, int* endOffset)
{
    Q_ASSERT(beforeAtAfter >= -1 && beforeAtAfter <= 1);
    Q_ASSERT(*startOffset == -1 && *endOffset == -1);
    int length = text.length();
    Q_ASSERT(offset >= 0 && offset <= length);

    // move offset into the right range (if asking for line before or after
    if (beforeAtAfter == 1) {
        offset = text.indexOf(QChar::LineFeed, qMin(offset, length - 1));
        if (offset < 0) {
            return QString(); // after the last line comes nothing
        }
        ++offset; // move after the newline
    } else if (beforeAtAfter == -1) {
        offset = text.lastIndexOf(QChar::LineFeed, qMax(offset - 1, 0));
        if (offset < 0) {
            return QString(); // before first line comes nothing
        }
    }

    if (offset > 0) {
        *startOffset = text.lastIndexOf(QChar::LineFeed, offset - 1);
    }
    ++*startOffset; // move to the char after the newline (0 if lastIndexOf returned -1)

    *endOffset = text.indexOf(QChar::LineFeed, qMin(offset, length - 1)) + 1; // include newline char
    if (*endOffset <= 0 || *endOffset > length) {
        *endOffset = length; // if the text doesn't end with a newline it ends at length
    }
    return text.mid(*startOffset, *endOffset - *startOffset);
}

//! NOTE: qaccessible.cpp - textBeforeOffset
QString AccessibleItem::accessibleTextBeforeOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const
{
    const QString txt = accessibleText(0, accessibleCharacterCount());

    if (offset == -1) {
        offset = txt.length();
    }

    *startOffset = *endOffset = -1;
    if (txt.isEmpty() || offset < 0 || offset >= txt.length()) {
        return QString();
    }

    // type initialized just to silence a compiler warning [-Werror=maybe-uninitialized]
    QTextBoundaryFinder::BoundaryType type = QTextBoundaryFinder::Grapheme;
    switch (boundaryType) {
    case TextBoundaryType::CharBoundary:
        type = QTextBoundaryFinder::Grapheme;
        break;
    case TextBoundaryType::WordBoundary:
        type = QTextBoundaryFinder::Word;
        break;
    case TextBoundaryType::SentenceBoundary:
        type = QTextBoundaryFinder::Sentence;
        break;
    case TextBoundaryType::LineBoundary:
    case TextBoundaryType::ParagraphBoundary:
        // Lines can not use QTextBoundaryFinder since Line there means any potential line-break.
        return textLineBoundary(-1, txt, offset, startOffset, endOffset);
    case TextBoundaryType::NoBoundary:
        // return empty, this function currently only supports single lines, so there can be no line after
        return QString();
    default:
        Q_UNREACHABLE();
    }

    // keep behavior in sync with QTextCursor::movePosition()!

    QTextBoundaryFinder boundary(type, txt);
    boundary.setPosition(offset);

    do {
        if ((boundary.boundaryReasons() & (QTextBoundaryFinder::StartOfItem | QTextBoundaryFinder::EndOfItem))) {
            break;
        }
    } while (boundary.toPreviousBoundary() > 0);
    Q_ASSERT(boundary.position() >= 0);
    *endOffset = boundary.position();

    while (boundary.toPreviousBoundary() > 0) {
        if ((boundary.boundaryReasons() & (QTextBoundaryFinder::StartOfItem | QTextBoundaryFinder::EndOfItem))) {
            break;
        }
    }
    Q_ASSERT(boundary.position() >= 0);
    *startOffset = boundary.position();

    return txt.mid(*startOffset, *endOffset - *startOffset);
}

//! NOTE: qaccessible.cpp - textAfterOffset
QString AccessibleItem::accessibleTextAfterOffset(int offset, TextBoundaryType boundaryType, int* startOffset, int* endOffset) const
{
    const QString txt = accessibleText(0, accessibleCharacterCount());

    if (offset == -1) {
        offset = txt.length();
    }

    *startOffset = *endOffset = -1;
    if (txt.isEmpty() || offset < 0 || offset >= txt.length()) {
        return QString();
    }

    // type initialized just to silence a compiler warning [-Werror=maybe-uninitialized]
    QTextBoundaryFinder::BoundaryType type = QTextBoundaryFinder::Grapheme;
    switch (boundaryType) {
    case TextBoundaryType::CharBoundary:
        type = QTextBoundaryFinder::Grapheme;
        break;
    case TextBoundaryType::WordBoundary:
        type = QTextBoundaryFinder::Word;
        break;
    case TextBoundaryType::SentenceBoundary:
        type = QTextBoundaryFinder::Sentence;
        break;
    case TextBoundaryType::LineBoundary:
    case TextBoundaryType::ParagraphBoundary:
        // Lines can not use QTextBoundaryFinder since Line there means any potential line-break.
        return textLineBoundary(1, txt, offset, startOffset, endOffset);
    case TextBoundaryType::NoBoundary:
        // return empty, this function currently only supports single lines, so there can be no line after
        return QString();
    default:
        Q_UNREACHABLE();
    }

    // keep behavior in sync with QTextCursor::movePosition()!

    QTextBoundaryFinder boundary(type, txt);
    boundary.setPosition(offset);

    while (true) {
        int toNext = boundary.toNextBoundary();
        if ((boundary.boundaryReasons() & (QTextBoundaryFinder::StartOfItem | QTextBoundaryFinder::EndOfItem))) {
            break;
        }
        if (toNext < 0 || toNext >= txt.length()) {
            break; // not found, the boundary might not exist
        }
    }
    Q_ASSERT(boundary.position() <= txt.length());
    *startOffset = boundary.position();

    while (true) {
        int toNext = boundary.toNextBoundary();
        if ((boundary.boundaryReasons() & (QTextBoundaryFinder::StartOfItem | QTextBoundaryFinder::EndOfItem))) {
            break;
        }
        if (toNext < 0 || toNext >= txt.length()) {
            break; // not found, the boundary might not exist
        }
    }
    Q_ASSERT(boundary.position() <= txt.length());
    *endOffset = boundary.position();

    if ((*startOffset == -1) || (*endOffset == -1) || (*startOffset == *endOffset)) {
        *endOffset = -1;
        *startOffset = -1;
    }

    return txt.mid(*startOffset, *endOffset - *startOffset);
}

QString AccessibleItem::accessibleTextAtOffset(int, TextBoundaryType, int* startOffset, int* endOffset) const
{
    //! NOTE: very simplified selection mode
    *startOffset = m_selectionStart;
    *endOffset = m_selectionEnd;

    return accessibleText(*startOffset, *endOffset);
}

int AccessibleItem::accessibleCharacterCount() const
{
    return text().size();
}

mu::async::Channel<IAccessible::Property, mu::Val> AccessibleItem::accessiblePropertyChanged() const
{
    return m_accessiblePropertyChanged;
}

mu::async::Channel<IAccessible::State, bool> AccessibleItem::accessibleStateChanged() const
{
    return m_accessibleStateChanged;
}

int AccessibleItem::accessibleRowIndex() const
{
    return m_row;
}

void AccessibleItem::classBegin()
{
}

void AccessibleItem::componentComplete()
{
    accessibilityController()->reg(this);
    m_registred = true;
}

AccessibleItem* AccessibleItem::accessibleParent_property() const
{
    return m_accessibleParent;
}

MUAccessible::Role AccessibleItem::role() const
{
    return m_role;
}

QString AccessibleItem::name() const
{
    return m_name;
}

QString AccessibleItem::description() const
{
    return m_description;
}

QVariant AccessibleItem::value() const
{
    return m_value;
}

QVariant AccessibleItem::maximumValue() const
{
    return m_maximumValue;
}

QVariant AccessibleItem::minimumValue() const
{
    return m_minimumValue;
}

QVariant AccessibleItem::stepSize() const
{
    return m_stepSize;
}

QString AccessibleItem::text() const
{
    return m_text;
}

QString AccessibleItem::selectedText() const
{
    return m_selectedText;
}

int AccessibleItem::selectionStart() const
{
    return m_selectionStart;
}

int AccessibleItem::selectionEnd() const
{
    return m_selectionEnd;
}

int AccessibleItem::cursorPosition() const
{
    return m_cursorPosition;
}

int AccessibleItem::row() const
{
    return m_row;
}

bool AccessibleItem::ignored() const
{
    return m_ignored;
}

QQuickItem* AccessibleItem::visualItem() const
{
    return m_visualItem;
}

QWindow* AccessibleItem::window() const
{
    return m_window;
}

void AccessibleItem::setState(IAccessible::State st, bool arg)
{
    if (m_state.value(st, false) == arg) {
        return;
    }

    m_state[st] = arg;
    emit stateChanged();

    if (!m_ignored) {
        m_accessibleStateChanged.send(st, arg);
    }
}

void AccessibleItem::setAccessibleParent(AccessibleItem* p)
{
    if (m_accessibleParent == p) {
        return;
    }

    if (m_accessibleParent) {
        m_accessibleParent->removeChild(this);
    }

    m_accessibleParent = p;

    if (m_accessibleParent) {
        m_accessibleParent->addChild(this);
    }

    emit accessiblePrnChanged();
    m_accessiblePropertyChanged.send(IAccessible::Property::Parent, Val());
}

void AccessibleItem::setRole(MUAccessible::Role role)
{
    if (m_role == role) {
        return;
    }

    m_role = role;
    emit roleChanged(m_role);

    m_state[State::Enabled] = true;
}

void AccessibleItem::setName(QString name)
{
    if (m_name == name) {
        return;
    }

    m_name = name;
    emit nameChanged(m_name);
    m_accessiblePropertyChanged.send(IAccessible::Property::Name, Val(name));
}

void AccessibleItem::setDescription(QString description)
{
    if (m_description == description) {
        return;
    }

    m_description = description;
    emit descriptionChanged(m_description);
    m_accessiblePropertyChanged.send(IAccessible::Property::Description, Val(description));
}

void AccessibleItem::setValue(QVariant value)
{
    if (m_value == value) {
        return;
    }

    m_value = value;
    emit valueChanged(m_value);
    m_accessiblePropertyChanged.send(IAccessible::Property::Value, Val::fromQVariant(value));
}

void AccessibleItem::setMaximumValue(QVariant maximumValue)
{
    if (m_maximumValue == maximumValue) {
        return;
    }

    m_maximumValue = maximumValue;
    emit maximumValueChanged(m_maximumValue);
}

void AccessibleItem::setMinimumValue(QVariant minimumValue)
{
    if (m_minimumValue == minimumValue) {
        return;
    }

    m_minimumValue = minimumValue;
    emit minimumValueChanged(m_minimumValue);
}

void AccessibleItem::setStepSize(QVariant stepSize)
{
    if (m_stepSize == stepSize) {
        return;
    }

    m_stepSize = stepSize;
    emit stepSizeChanged(m_stepSize);
}

void AccessibleItem::setText(const QString& text)
{
    if (m_text == text) {
        return;
    }

    m_text = text;

#if defined(Q_OS_MACOS)
    //! NOTE: For VoiceOver, text must also be in value
    setValue(text);
#endif

    emit textChanged();
}

void AccessibleItem::setSelectedText(const QString& selectedText)
{
    if (m_selectedText == selectedText) {
        return;
    }

    m_selectedText = selectedText;
    emit selectedTextChanged();
}

void AccessibleItem::setSelectionStart(int selectionStart)
{
    if (m_selectionStart == selectionStart) {
        return;
    }

    m_selectionStart = selectionStart;
    emit selectionStartChanged();
}

void AccessibleItem::setSelectionEnd(int selectionEnd)
{
    if (m_selectionEnd == selectionEnd) {
        return;
    }

    m_selectionEnd = selectionEnd;
    emit selectionEndChanged();
}

void AccessibleItem::setCursorPosition(int cursorPosition)
{
    if (m_cursorPosition == cursorPosition) {
        return;
    }

    m_cursorPosition = cursorPosition;
    emit cursorPositionChanged();
    m_accessiblePropertyChanged.send(IAccessible::Property::TextCursor, Val());
}

void AccessibleItem::setRow(int row)
{
    if (m_row == row) {
        return;
    }

    m_row = row;
    emit rowChanged();
}

void AccessibleItem::setIgnored(bool ignored)
{
    if (m_ignored == ignored) {
        return;
    }

    m_ignored = ignored;
    emit ignoredChanged(m_ignored);
}

void AccessibleItem::setVisualItem(QQuickItem* item)
{
    if (m_visualItem == item) {
        return;
    }

    m_visualItem = item;
    emit visualItemChanged(item);
}

void AccessibleItem::setWindow(QWindow* window)
{
    if (m_window == window) {
        return;
    }

    m_window = window;
    emit windowChanged();
}

const IAccessible* AccessibleItem::accessibleRoot() const
{
    return accessibilityController()->accessibleRoot();
}

void AccessibleItem::addChild(AccessibleItem* item)
{
    m_children.append(item);
}

void AccessibleItem::removeChild(AccessibleItem* item)
{
    m_children.removeOne(item);
}

QQuickItem* AccessibleItem::resolveVisualItem() const
{
    if (m_visualItem) {
        return m_visualItem;
    }

    QObject* prn = parent();
    while (prn) {
        QQuickItem* vitem = qobject_cast<QQuickItem*>(prn);
        if (vitem) {
            return vitem;
        }
        prn = prn->parent();
    }

    return nullptr;
}
