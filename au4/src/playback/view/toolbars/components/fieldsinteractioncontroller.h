/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QQuickItem>

#include "timecodeformatter.h"

namespace au::playback {
class FieldsInteractionController : public QObject
{
    Q_OBJECT

public:
    explicit FieldsInteractionController(QObject* parent = nullptr);

    void setFormatter(const std::shared_ptr<TimecodeFormatter>& formatter);

    QString valueString() const;
    void setValueString(const QString& valueString);

    int currentEditedFieldIndex() const;
    void setCurrentEditedFieldIndex(int index);

    QQuickItem* visualItem() const;
    void setVisualItem(QQuickItem* newVisualItem);

    int rowCount() const;

signals:
    void currentEditedFieldIndexChanged();
    void visualItemChanged();

    void valueChanged(double value);

private:
    bool eventFilter(QObject* watched, QEvent* event) override;
    bool needOverrideShortcut(QEvent* event) const;

    bool isMouseWithinBoundaries(const QPoint& mousePos) const;

    void moveCurrentEditedField(int moveKey);
    void adjustCurrentEditedField(int adjustKey);
    void scrollCurrentEditedField(int pixelsYScrolled, int stepsYScrolled);

    void finishEditing();

    QString m_valueString;

    int m_currentEditedFieldIndex = -1;

    QQuickItem* m_visualItem = nullptr;

    std::shared_ptr<TimecodeFormatter> m_formatter;

    int m_scrolled = 0;
};
}
