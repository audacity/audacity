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
class ValueWarper : public QObject
{
    Q_OBJECT

    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)
    Q_PROPERTY(double min READ min WRITE setMin NOTIFY minChanged FINAL)
    Q_PROPERTY(double max READ max WRITE setMax NOTIFY maxChanged FINAL)
    Q_PROPERTY(ValueWarpingType warpingType READ warpingType WRITE setWarpingType NOTIFY warpingTypeChanged FINAL)

    // Bind this to the control value you want to warp.
    Q_PROPERTY(double warpedValue READ warpedValue WRITE setWarpedValue NOTIFY warpedValueChanged FINAL)

public:
    explicit ValueWarper(QObject* parent = nullptr)
        : QObject(parent) {}

    Q_INVOKABLE void init();

    ValueWarpingType warpingType() const;
    void setWarpingType(ValueWarpingType type);

    double value() const;
    void setValue(double value);

    double warpedValue() const;
    void setWarpedValue(double value);

    double min() const;
    void setMin(double value);

    double max() const;
    void setMax(double value);

signals:
    void valueChanged();
    void warpedValueChanged();
    void minChanged();
    void maxChanged();
    void warpingTypeChanged();

private:
    double m_value = 0.0;
    double m_warpedValue = 0.0;
    double m_min = 0.0;
    double m_max = 1.0;
    ValueWarpingType m_warpingType = ValueWarpingType::None;
    std::unique_ptr<IValueTransformer> m_transformer;
};
} // namespace au::effects
