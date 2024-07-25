/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QString>
#include <QColor>

#include "processing/dom/clip.h"
#include "../../types/projectscenetypes.h"

namespace au::projectscene {
class ClipListItem : public QObject
{
    Q_OBJECT

    Q_PROPERTY(ClipKey key READ key CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged FINAL)
    Q_PROPERTY(QColor color READ color CONSTANT)

    Q_PROPERTY(double x READ x WRITE setX NOTIFY xChanged FINAL)
    Q_PROPERTY(double width READ width WRITE setWidth NOTIFY widthChanged FINAL)

    Q_PROPERTY(ClipTime time READ time WRITE setTime NOTIFY timeChanged FINAL)

    Q_PROPERTY(double moveMaximumX READ moveMaximumX WRITE setMoveMaximumX NOTIFY moveMaximumXChanged FINAL)
    Q_PROPERTY(double moveMinimumX READ moveMinimumX WRITE setMoveMinimumX NOTIFY moveMinimumXChanged FINAL)

    Q_PROPERTY(bool selected READ selected WRITE setSelected NOTIFY selectedChanged FINAL)

public:
    ClipListItem(QObject* parent);

    void setClip(const processing::Clip& clip);
    const processing::Clip& clip() const;

    ClipKey key() const;
    QString title() const;
    void setTitle(const QString& newTitle);

    QColor color() const;

    double x() const;
    void setX(double newX);

    double width() const;
    void setWidth(double newWidth);

    double moveMaximumX() const;
    void setMoveMaximumX(double newMoveMaximumX);

    double moveMinimumX() const;
    void setMoveMinimumX(double newMoveMinimumX);

    bool selected() const;
    void setSelected(bool newSelected);

    ClipTime time() const;
    void setTime(const ClipTime &newTime);

signals:
    void titleChanged();
    void xChanged();
    void widthChanged();
    void moveMaximumXChanged();
    void moveMinimumXChanged();
    void selectedChanged();

    void timeChanged();

private:
    processing::Clip m_clip;
    double m_x = 0.0;
    double m_width = 0.0;
    double m_moveMaximumX = 0.0;
    double m_moveMinimumX = 0.0;
    bool m_selected = false;
    ClipTime m_time;
};
}
