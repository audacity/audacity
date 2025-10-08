/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QString>
#include <QColor>

#include "trackedit/dom/label.h"

namespace au::projectscene {
class LabelKey // todo: change to TrackItemKey
{
    Q_GADGET

public:

    LabelKey() = default;
    LabelKey(const trackedit::LabelKey& k)
        : key(k) {}

    trackedit::LabelKey key;
};

class LabelTime  // todo: change to TrackItemTime
{
    Q_GADGET

public:

    double labelStartTime = 0.0;
    double labelEndTime = 0.0;

    double itemStartTime = 0.0;
    double itemEndTime = 0.0;

    double selectionStartTime = 0.0;
    double selectionEndTime = 0.0;

    inline bool operator==(const LabelTime& other) const
    {
        return muse::is_equal(labelStartTime, other.labelStartTime)
               && muse::is_equal(labelEndTime, other.labelEndTime)
               && muse::is_equal(itemStartTime, other.itemStartTime)
               && muse::is_equal(itemEndTime, other.itemEndTime)
               && muse::is_equal(selectionStartTime, other.selectionStartTime)
               && muse::is_equal(selectionEndTime, other.selectionEndTime);
    }

    inline bool operator!=(const LabelTime& other) const { return !this->operator==(other); }
};

class LabelListItem : public QObject
{
    Q_OBJECT

    Q_PROPERTY(LabelKey key READ key CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged FINAL)
    Q_PROPERTY(QColor color READ color NOTIFY colorChanged FINAL)

    Q_PROPERTY(double x READ x WRITE setX NOTIFY xChanged FINAL)
    Q_PROPERTY(double width READ width WRITE setWidth NOTIFY widthChanged FINAL)

    Q_PROPERTY(double leftVisibleMargin READ leftVisibleMargin WRITE setLeftVisibleMargin NOTIFY leftVisibleMarginChanged FINAL)
    Q_PROPERTY(double rightVisibleMargin READ rightVisibleMargin WRITE setRightVisibleMargin NOTIFY rightVisibleMarginChanged FINAL)

    Q_PROPERTY(LabelTime time READ time WRITE setTime NOTIFY timeChanged FINAL)

    Q_PROPERTY(bool selected READ selected WRITE setSelected NOTIFY selectedChanged FINAL)

public:
    explicit LabelListItem(QObject* parent);

    void setLabel(const trackedit::Label& label);
    const trackedit::Label& label() const;

    LabelKey key() const;
    QString title() const;
    void setTitle(const QString& newTitle);

    QColor color() const;

    double x() const;
    void setX(double newX);

    double width() const;
    void setWidth(double newWidth);

    bool selected() const;
    void setSelected(bool newSelected);

    LabelTime time() const;
    void setTime(const LabelTime& newTime);

    double leftVisibleMargin() const;
    void setLeftVisibleMargin(double newLeftVisibleMargin);

    double rightVisibleMargin() const;
    void setRightVisibleMargin(double newRightVisibleMargin);

signals:
    void titleChanged();
    void xChanged();
    void widthChanged();
    void colorChanged();
    void leftVisibleMarginChanged();
    void rightVisibleMarginChanged();
    void timeChanged();
    void selectedChanged();
    void titleEditRequested();

private:
    trackedit::Label m_label;
    double m_x = 0.0;
    double m_width = 0.0;
    bool m_selected = false;
    LabelTime m_time;
    double m_leftVisibleMargin = 0.0;
    double m_rightVisibleMargin = 0.0;
};
}
