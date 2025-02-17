/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QString>
#include <QColor>

#include "trackedit/dom/clip.h"
#include "../../types/projectscenetypes.h"

namespace au::projectscene {
class ClipListItem : public QObject
{
    Q_OBJECT

    Q_PROPERTY(ClipKey key READ key CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged FINAL)
    Q_PROPERTY(QColor color READ color NOTIFY colorChanged FINAL)
    Q_PROPERTY(int groupId READ groupId NOTIFY groupIdChanged FINAL)

    Q_PROPERTY(double x READ x WRITE setX NOTIFY xChanged FINAL)
    Q_PROPERTY(double width READ width WRITE setWidth NOTIFY widthChanged FINAL)

    Q_PROPERTY(double leftVisibleMargin READ leftVisibleMargin WRITE setLeftVisibleMargin NOTIFY leftVisibleMarginChanged FINAL)
    Q_PROPERTY(double rightVisibleMargin READ rightVisibleMargin WRITE setRightVisibleMargin NOTIFY rightVisibleMarginChanged FINAL)

    Q_PROPERTY(ClipTime time READ time WRITE setTime NOTIFY timeChanged FINAL)

    Q_PROPERTY(bool selected READ selected WRITE setSelected NOTIFY selectedChanged FINAL)

    Q_PROPERTY(int pitch READ pitch NOTIFY pitchChanged FINAL)
    Q_PROPERTY(int speedPercentage READ speedPercentage NOTIFY speedPercentageChanged FINAL)

public:
    ClipListItem(QObject* parent);

    void setClip(const trackedit::Clip& clip);
    const trackedit::Clip& clip() const;

    ClipKey key() const;
    QString title() const;
    void setTitle(const QString& newTitle);

    QColor color() const;

    int groupId() const;

    double x() const;
    void setX(double newX);

    double width() const;
    void setWidth(double newWidth);

    bool selected() const;
    void setSelected(bool newSelected);

    ClipTime time() const;
    void setTime(const ClipTime& newTime);

    double leftVisibleMargin() const;
    void setLeftVisibleMargin(double newLeftVisibleMargin);

    double rightVisibleMargin() const;
    void setRightVisibleMargin(double newRightVisibleMargin);

    int pitch() const;
    int speedPercentage() const;

signals:
    void titleChanged();
    void xChanged();
    void widthChanged();
    void colorChanged();
    void groupIdChanged();
    void leftVisibleMarginChanged();
    void rightVisibleMarginChanged();
    void timeChanged();
    void selectedChanged();
    void pitchChanged();
    void speedPercentageChanged();

private:
    trackedit::Clip m_clip;
    double m_x = 0.0;
    double m_width = 0.0;
    bool m_selected = false;
    ClipTime m_time;
    double m_leftVisibleMargin = 0.0;
    double m_rightVisibleMargin = 0.0;
};
}
