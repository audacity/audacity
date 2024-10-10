/*
* Audacity: A Digital Audio Editor
*/
#include "fieldsinteractioncontroller.h"

#include <QKeyEvent>

#include "log.h"

using namespace au::playback;

static bool isFieldEditable(const QChar& fieldSymbol)
{
    return fieldSymbol.isDigit();
}

FieldsInteractionController::FieldsInteractionController(QObject* parent)
    : QObject(parent)
{
}

void FieldsInteractionController::setFormatter(const std::shared_ptr<TimecodeFormatter>& formatter)
{
    m_formatter = formatter;
}

QString FieldsInteractionController::valueString() const
{
    return m_valueString;
}

void FieldsInteractionController::setValueString(const QString& valueString)
{
    m_valueString = valueString;
}

int FieldsInteractionController::currentEditedFieldIndex() const
{
    return m_currentEditedFieldIndex;
}

void FieldsInteractionController::setCurrentEditedFieldIndex(int index)
{
    if (m_currentEditedFieldIndex == index) {
        return;
    }

    if (index >= 0) {
        qApp->installEventFilter(this);
    } else {
        qApp->removeEventFilter(this);
    }

    m_currentEditedFieldIndex = index;
    emit currentEditedFieldIndexChanged();
}

QQuickItem* FieldsInteractionController::visualItem() const
{
    return m_visualItem;
}

void FieldsInteractionController::setVisualItem(QQuickItem* newVisualItem)
{
    if (m_visualItem == newVisualItem) {
        return;
    }

    m_visualItem = newVisualItem;
    emit visualItemChanged();
}

int FieldsInteractionController::rowCount() const
{
    return m_valueString.size();
}

bool FieldsInteractionController::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::ShortcutOverride) {
        bool needOverride = needOverrideShortcut(event);
        if (needOverride) {
            event->accept();
            return true;
        } else {
            finishEditing();
            return false;
        }
    }

    if (event->type() == QEvent::KeyPress) {
        QKeyEvent* keyEvent = dynamic_cast<QKeyEvent*>(event);
        int key = keyEvent->key();
        if (key == Qt::Key_Escape) {
            finishEditing();
            return true;
        } else if (key >= Qt::Key_0 && key <= Qt::Key_9) {
            QString newValueStr = m_valueString;
            newValueStr.replace(m_currentEditedFieldIndex, 1, QChar('0' + (key - Qt::Key_0)));

            emit valueChanged(m_formatter->stringToValue(newValueStr).value());

            moveCurrentEditedField(Qt::Key_Right);

            return true;
        } else if (key == Qt::Key_Left || key == Qt::Key_Right) {
            moveCurrentEditedField(key);
            return true;
        } else if (key == Qt::Key_Up || key == Qt::Key_Down) {
            adjustCurrentEditedField(key);
            return true;
        }
    } else if (event->type() == QEvent::MouseButtonPress) {
        if (!isMouseWithinBoundaries(QCursor::pos())) {
            finishEditing();
        }
    } else if (event->type() == QEvent::Wheel && isMouseWithinBoundaries(QCursor::pos())) {
        QWheelEvent* wheelEvent = dynamic_cast<QWheelEvent*>(event);
        scrollCurrentEditedField(wheelEvent->pixelDelta().y(), wheelEvent->angleDelta().y());
        return true;
    }

    return QObject::eventFilter(watched, event);
}

bool FieldsInteractionController::needOverrideShortcut(QEvent* event) const
{
    int key = dynamic_cast<QKeyEvent*>(event)->key();

    bool isDigit = key >= Qt::Key_0 && key <= Qt::Key_9;
    if (isDigit) {
        return true;
    }

    static const std::set<int> navigationKeys {
        Qt::Key_Left,
        Qt::Key_Right,
        Qt::Key_Up,
        Qt::Key_Down,

        Qt::Key_Escape,
        Qt::Key_Enter,
        Qt::Key_Return
    };

    return muse::contains(navigationKeys, key);
}

bool FieldsInteractionController::isMouseWithinBoundaries(const QPoint& mousePos) const
{
    if (!m_visualItem) {
        return false;
    }

    QRectF viewRect = m_visualItem->boundingRect();
    viewRect.moveTopLeft(m_visualItem->mapToGlobal(QPoint(0, 0)));

    return viewRect.contains(mousePos);
}

void FieldsInteractionController::moveCurrentEditedField(int moveKey)
{
    int newIndex = m_currentEditedFieldIndex;
    while (true) {
        if (moveKey == Qt::Key_Left) {
            newIndex--;
        } else {
            newIndex++;
        }

        if (newIndex < 0) {
            newIndex = rowCount() - 1;
        } else if (newIndex >= rowCount()) {
            newIndex = 0;
        }

        if (isFieldEditable(m_valueString[newIndex])) {
            break;
        }
    }

    setCurrentEditedFieldIndex(newIndex);
}

void FieldsInteractionController::adjustCurrentEditedField(int adjustKey)
{
    int digitIndex = 0;

    for (int i = 0; i < m_valueString.size(); ++i) {
        if (i == m_currentEditedFieldIndex) {
            break;
        }

        if (isFieldEditable(m_valueString[i])) {
            digitIndex++;
        }
    }

    emit valueChanged(m_formatter->singleStep(m_valueString, digitIndex, adjustKey == Qt::Key_Up));
}

void FieldsInteractionController::scrollCurrentEditedField(int pixelsYScrolled, int stepsYScrolled)
{
    int oneScroll = -1;

    if (pixelsYScrolled != 0) {
        m_scrolled += pixelsYScrolled;
        oneScroll = QWheelEvent::DefaultDeltasPerStep / 2;
    } else if (stepsYScrolled != 0) {
        m_scrolled += stepsYScrolled;
        oneScroll = QWheelEvent::DefaultDeltasPerStep;
    }

    if (oneScroll == -1) {
        return;
    }

    if (m_scrolled >= oneScroll) {
        adjustCurrentEditedField(Qt::Key_Up);
        m_scrolled = 0;
    } else if (m_scrolled <= -oneScroll) {
        adjustCurrentEditedField(Qt::Key_Down);
        m_scrolled = 0;
    }
}

void FieldsInteractionController::finishEditing()
{
    setCurrentEditedFieldIndex(-1);
}
