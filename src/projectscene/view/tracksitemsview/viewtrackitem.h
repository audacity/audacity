/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QString>
#include <QColor>

#include "projectscene/types/projectscenetypes.h"

namespace au::projectscene {
class ViewTrackItem : public QObject
{
    Q_OBJECT

    Q_PROPERTY(TrackItemKey key READ key CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged FINAL)
    Q_PROPERTY(QColor color READ color NOTIFY colorChanged FINAL)
    Q_PROPERTY(double x READ x WRITE setX NOTIFY xChanged FINAL)
    Q_PROPERTY(double width READ width WRITE setWidth NOTIFY widthChanged FINAL)
    Q_PROPERTY(double leftVisibleMargin READ leftVisibleMargin WRITE setLeftVisibleMargin NOTIFY leftVisibleMarginChanged FINAL)
    Q_PROPERTY(double rightVisibleMargin READ rightVisibleMargin WRITE setRightVisibleMargin NOTIFY rightVisibleMarginChanged FINAL)
    Q_PROPERTY(TrackItemTime time READ time WRITE setTime NOTIFY timeChanged FINAL)
    Q_PROPERTY(bool selected READ selected WRITE setSelected NOTIFY selectedChanged FINAL)
    Q_PROPERTY(bool intersectsSelection READ intersectsSelection WRITE setIntersectsSelection NOTIFY intersectsSelectionChanged FINAL)
    Q_PROPERTY(bool focused READ focused WRITE setFocused NOTIFY focusedChanged FINAL)

public:
    explicit ViewTrackItem(QObject* parent = nullptr);
    ~ViewTrackItem() override;

    TrackItemKey key() const;

    QString title() const;
    void setTitle(const QString& newTitle);
    QColor color() const;

    double x() const;
    void setX(double newX);

    double width() const;
    void setWidth(double newWidth);

    bool selected() const;
    void setSelected(bool newSelected);

    bool intersectsSelection() const;
    void setIntersectsSelection(bool newState);

    bool focused() const;
    void setFocused(bool focused);

    double leftVisibleMargin() const;
    void setLeftVisibleMargin(double newLeftVisibleMargin);

    double rightVisibleMargin() const;
    void setRightVisibleMargin(double newRightVisibleMargin);

    TrackItemTime time() const;
    void setTime(const TrackItemTime& newTime);

signals:
    void titleChanged();
    void xChanged();
    void widthChanged();
    void colorChanged();
    void leftVisibleMarginChanged();
    void rightVisibleMarginChanged();

    void timeChanged();
    void startTimeChanged();
    void endTimeChanged();

    void selectedChanged();
    void intersectsSelectionChanged();

    void focusedChanged();

protected:
    TrackItemKey m_key;
    QString m_title;
    QColor m_color;
    double m_x = 0.0;
    double m_width = 0.0;
    bool m_selected = false;
    bool m_intersectsSelection = false;
    bool m_focused = false;
    double m_leftVisibleMargin = 0.0;
    double m_rightVisibleMargin = 0.0;
    TrackItemTime m_time;
};
}
