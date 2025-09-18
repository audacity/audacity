/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "valuewarpertypes.h"

#include <QObject>

namespace au::effects {
/*!
 * \brief Warps `value` to `warpedValue`, i.e., if `value == min`, then `warpedValue == min`, and likewise for `max`.
 * The `warpedValue` of `ValueWarper` and the value of the control to be warped must be bound together and to nothing else.
 * `min` and `max` are the limits of the control.
 */
class ValueWarper : public QObject, public QQmlParserStatus
{
    Q_OBJECT

    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)
    Q_PROPERTY(double min READ min WRITE setMin NOTIFY minChanged FINAL)
    Q_PROPERTY(double max READ max WRITE setMax NOTIFY maxChanged FINAL)
    Q_PROPERTY(QVariant middle READ middle WRITE setMiddle NOTIFY middleChanged FINAL)

    // Bind this to the control value you want to warp.
    Q_PROPERTY(double warpedValue READ warpedValue WRITE setWarpedValue NOTIFY warpedValueChanged FINAL)

public:
    explicit ValueWarper(QObject* parent = nullptr)
        : QObject(parent) {}

    double value() const;
    void setValue(double value);

    double warpedValue() const;
    void setWarpedValue(double value);

    double min() const;
    void setMin(double value);

    double max() const;
    void setMax(double value);

    QVariant middle() const;
    void setMiddle(const QVariant& value);

signals:
    void valueChanged();
    void warpedValueChanged();
    void minChanged();
    void maxChanged();
    void middleChanged();
    void warpingTypeChanged();

private:
    void classBegin() override {}
    void componentComplete() override;
    bool useWarper() const;

    double m_value = 0.0;
    double m_warpedValue = 0.0;
    double m_min = 0.0;
    double m_max = 1.0;
    QVariant m_middle; // null would mean passthrough
    std::unique_ptr<IValueTransformer> m_transformer;
};
} // namespace au::effects
