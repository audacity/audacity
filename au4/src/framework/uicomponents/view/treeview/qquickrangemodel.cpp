/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** This file is part of the Qt Quick Controls module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 3 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL3 included in the
** packaging of this file. Please review the following information to
** ensure the GNU Lesser General Public License version 3 requirements
** will be met: https://www.gnu.org/licenses/lgpl-3.0.html.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 2.0 or (at your option) the GNU General
** Public license version 3 or any later version approved by the KDE Free
** Qt Foundation. The licenses are as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL2 and LICENSE.GPL3
** included in the packaging of this file. Please review the following
** information to ensure the GNU General Public License requirements will
** be met: https://www.gnu.org/licenses/gpl-2.0.html and
** https://www.gnu.org/licenses/gpl-3.0.html.
**
** $QT_END_LICENSE$
**
****************************************************************************/

/*!
    With this class, the user sets a value range and a position range, which
    represent the valid values/positions the model can assume. It is worth telling
    that the value property always has priority over the position property. A nice use
    case, would be a Slider implementation with the help of QQuickRangeModel. If the user sets
    a value range to [0,100], a position range to [50,100] and sets the value
    to 80, the equivalent position would be 90. After that, if the user decides to
    resize the slider, the value would be the same, but the knob position would
    be updated due to the new position range.
*/

#include "qquickrangemodel_p.h"
#include "qquickrangemodel_p_p.h"

QT_BEGIN_NAMESPACE

#define Q_PD(Class) Class##Private * const pd = d_func()

QQuickRangeModel1Private::QQuickRangeModel1Private(QQuickRangeModel1 *qq)
    : q_ptr(qq)
{
}

QQuickRangeModel1Private::~QQuickRangeModel1Private()
{
}

void QQuickRangeModel1Private::init()
{
    minimum = 0;
    maximum = 99;
    stepSize = 0;
    value = 0;
    pos = 0;
    posatmin = 0;
    posatmax = 0;
    inverted = false;
    isComplete = false;
    positionChanged = false;
    valueChanged = false;
}

/*!
    Calculates the position that is going to be seen outside by the component
    that is using QQuickRangeModel. It takes into account the \l stepSize,
    \l positionAtMinimum, \l positionAtMaximum properties
    and \a position that is passed as parameter.
*/

qreal QQuickRangeModel1Private::publicPosition(qreal position) const
{
    // Calculate the equivalent stepSize for the position property.
    const qreal min = effectivePosAtMin();
    const qreal max = effectivePosAtMax();
    const qreal valueRange = maximum - minimum;
    const qreal positionValueRatio = valueRange ? (max - min) / valueRange : 0;
    const qreal positionStep = stepSize * positionValueRatio;

    if (positionStep == 0)
        return (min < max) ? qBound(min, position, max) : qBound(max, position, min);

    const int stepSizeMultiplier = (position - min) / positionStep;

    // Test whether value is below minimum range
    if (stepSizeMultiplier < 0)
        return min;

    qreal leftEdge = (stepSizeMultiplier * positionStep) + min;
    qreal rightEdge = ((stepSizeMultiplier + 1) * positionStep) + min;

    if (min < max) {
        leftEdge = qMin(leftEdge, max);
        rightEdge = qMin(rightEdge, max);
    } else {
        leftEdge = qMax(leftEdge, max);
        rightEdge = qMax(rightEdge, max);
    }

    if (qAbs(leftEdge - position) <= qAbs(rightEdge - position))
        return leftEdge;
    return rightEdge;
}

/*!
    Calculates the value that is going to be seen outside by the component
    that is using QQuickRangeModel. It takes into account the \l stepSize,
    \l minimumValue, \l maximumValue properties
    and \a value that is passed as parameter.
*/

qreal QQuickRangeModel1Private::publicValue(qreal pValue) const
{
    // It is important to do value-within-range check this
    // late (as opposed to during setPosition()). The reason is
    // QML bindings; a position that is initially invalid because it lays
    // outside the range, might become valid later if the range changes.

    if (stepSize == 0)
        return qBound(minimum, pValue, maximum);

    const int stepSizeMultiplier = (pValue - minimum) / stepSize;

    // Test whether value is below minimum range
    if (stepSizeMultiplier < 0)
        return minimum;

    const qreal leftEdge = qMin(maximum, (stepSizeMultiplier * stepSize) + minimum);
    const qreal rightEdge = qMin(maximum, ((stepSizeMultiplier + 1) * stepSize) + minimum);
    const qreal middle = (leftEdge + rightEdge) / 2;

    return (pValue <= middle) ? leftEdge : rightEdge;
}

/*!
    Checks if the \l value or \l position, that is seen by the user, has changed and emits the changed signal if it
    has changed.
*/

void QQuickRangeModel1Private::emitValueAndPositionIfChanged(const qreal oldValue, const qreal oldPosition)
{
    Q_Q(QQuickRangeModel1);

    // Effective value and position might have changed even in cases when e.g. d->value is
    // unchanged. This will be the case when operating with values outside range:
    const qreal newValue = q->value();
    const qreal newPosition = q->position();

    if (isComplete) {
        if (!qFuzzyCompare(newValue, oldValue))
            emit q->valueChanged(newValue);
        if (!qFuzzyCompare(newPosition, oldPosition))
            emit q->positionChanged(newPosition);
    } else {
        positionChanged |= qFuzzyCompare(oldPosition, newPosition);
        valueChanged |= !qFuzzyCompare(oldValue, newValue);
    }
}

/*!
    Constructs a QQuickRangeModel with \a parent
*/

QQuickRangeModel1::QQuickRangeModel1(QObject *parent)
    : QObject(parent), d_ptr(new QQuickRangeModel1Private(this))
{
    Q_PD(QQuickRangeModel1);
    pd->init();
}

/*!
    \internal
    Constructs a QQuickRangeModel with private class pointer \a dd and \a parent
*/

QQuickRangeModel1::QQuickRangeModel1(QQuickRangeModel1Private &dd, QObject *parent)
    : QObject(parent), d_ptr(&dd)
{
    Q_PD(QQuickRangeModel1);
    pd->init();
}

/*!
    Destroys the QQuickRangeModel
*/

QQuickRangeModel1::~QQuickRangeModel1()
{
    delete d_ptr;
    d_ptr = 0;
}

/*!
    Sets the range of valid positions, that \l position can assume externally, with
    \a min and \a max.
    Such range is represented by \l positionAtMinimum and \l positionAtMaximum
*/

void QQuickRangeModel1::setPositionRange(qreal min, qreal max)
{
    Q_PD(QQuickRangeModel1);

    bool emitPosAtMinChanged = !qFuzzyCompare(min, pd->posatmin);
    bool emitPosAtMaxChanged = !qFuzzyCompare(max, pd->posatmax);

    if (!(emitPosAtMinChanged || emitPosAtMaxChanged))
        return;

    const qreal oldPosition = position();
    pd->posatmin = min;
    pd->posatmax = max;

    // When a new positionRange is defined, the position property must be updated based on the value property.
    // For instance, imagine that you have a valueRange of [0,100] and a position range of [20,100],
    // if a user set the value to 50, the position would be 60. If this positionRange is updated to [0,100], then
    // the new position, based on the value (50), will be 50.
    // If the newPosition is different than the old one, it must be updated, in order to emit
    // the positionChanged signal.
    pd->pos = pd->equivalentPosition(pd->value);

    if (emitPosAtMinChanged)
        emit positionAtMinimumChanged(pd->posatmin);
    if (emitPosAtMaxChanged)
        emit positionAtMaximumChanged(pd->posatmax);

    pd->emitValueAndPositionIfChanged(value(), oldPosition);
}
/*!
    Sets the range of valid values, that \l value can assume externally, with
    \a min and \a max. The range has the following constraint: \a min must be less or equal \a max
    Such range is represented by \l minimumValue and \l maximumValue
*/

void QQuickRangeModel1::setRange(qreal min, qreal max)
{
    Q_PD(QQuickRangeModel1);

    bool emitMinimumChanged = !qFuzzyCompare(min, pd->minimum);
    bool emitMaximumChanged = !qFuzzyCompare(max, pd->maximum);

    if (!(emitMinimumChanged || emitMaximumChanged))
        return;

    const qreal oldValue = value();
    const qreal oldPosition = position();

    pd->minimum = min;
    pd->maximum = qMax(min, max);

    // Update internal position if it was changed. It can occur if internal value changes, due to range update
    pd->pos = pd->equivalentPosition(pd->value);

    if (emitMinimumChanged)
        emit minimumChanged(pd->minimum);
    if (emitMaximumChanged)
        emit maximumChanged(pd->maximum);

    pd->emitValueAndPositionIfChanged(oldValue, oldPosition);
}

/*!
    \property QQuickRangeModel1::minimumValue
    \brief the minimum value that \l value can assume

    This property's default value is 0
*/

void QQuickRangeModel1::setMinimum(qreal min)
{
    Q_PD(const QQuickRangeModel1);
    setRange(min, pd->maximum);
}

qreal QQuickRangeModel1::minimum() const
{
    Q_PD(const QQuickRangeModel1);
    return pd->minimum;
}

/*!
    \property QQuickRangeModel1::maximumValue
    \brief the maximum value that \l value can assume

    This property's default value is 99
*/

void QQuickRangeModel1::setMaximum(qreal max)
{
    Q_PD(const QQuickRangeModel1);
    // if the new maximum value is smaller than
    // minimum, update minimum too
    setRange(qMin(pd->minimum, max), max);
}

qreal QQuickRangeModel1::maximum() const
{
    Q_PD(const QQuickRangeModel1);
    return pd->maximum;
}

/*!
    \property QQuickRangeModel1::stepSize
    \brief the value that is added to the \l value and \l position property

    Example: If a user sets a range of [0,100] and stepSize
    to 30, the valid values that are going to be seen externally would be: 0, 30, 60, 90, 100.
*/

void QQuickRangeModel1::setStepSize(qreal stepSize)
{
    Q_PD(QQuickRangeModel1);

    stepSize = qMax(qreal(0.0), stepSize);
    if (qFuzzyCompare(stepSize, pd->stepSize))
        return;

    const qreal oldValue = value();
    const qreal oldPosition = position();
    pd->stepSize = stepSize;

    emit stepSizeChanged(pd->stepSize);
    pd->emitValueAndPositionIfChanged(oldValue, oldPosition);
}

qreal QQuickRangeModel1::stepSize() const
{
    Q_PD(const QQuickRangeModel1);
    return pd->stepSize;
}

/*!
    Returns a valid position, respecting the \l positionAtMinimum,
    \l positionAtMaximum and the \l stepSize properties.
    Such calculation is based on the parameter \a value (which is valid externally).
*/

qreal QQuickRangeModel1::positionForValue(qreal value) const
{
    Q_PD(const QQuickRangeModel1);

    const qreal unconstrainedPosition = pd->equivalentPosition(value);
    return pd->publicPosition(unconstrainedPosition);
}

void QQuickRangeModel1::classBegin()
{
}

void QQuickRangeModel1::componentComplete()
{
    Q_PD(QQuickRangeModel1);
    pd->isComplete = true;
    emit minimumChanged(minimum());
    emit maximumChanged(maximum());
    if (pd->valueChanged)
        emit valueChanged(value());
    if (pd->positionChanged)
        emit positionChanged(position());
}

/*!
    \property QQuickRangeModel1::position
    \brief the current position of the model

    Represents a valid external position, based on the \l positionAtMinimum,
    \l positionAtMaximum and the \l stepSize properties.
    The user can set it internally with a position, that is not within the current position range,
    since it can become valid if the user changes the position range later.
*/

qreal QQuickRangeModel1::position() const
{
    Q_PD(const QQuickRangeModel1);

    // Return the internal position but observe boundaries and
    // stepSize restrictions.
    return pd->publicPosition(pd->pos);
}

void QQuickRangeModel1::setPosition(qreal newPosition)
{
    Q_PD(QQuickRangeModel1);

    if (qFuzzyCompare(newPosition, pd->pos))
        return;

    const qreal oldPosition = position();
    const qreal oldValue = value();

    // Update position and calculate new value
    pd->pos = newPosition;
    pd->value = pd->equivalentValue(pd->pos);
    pd->emitValueAndPositionIfChanged(oldValue, oldPosition);
}

/*!
    \property QQuickRangeModel1::positionAtMinimum
    \brief the minimum value that \l position can assume

    This property's default value is 0
*/

void QQuickRangeModel1::setPositionAtMinimum(qreal min)
{
    Q_PD(QQuickRangeModel1);
    setPositionRange(min, pd->posatmax);
}

qreal QQuickRangeModel1::positionAtMinimum() const
{
    Q_PD(const QQuickRangeModel1);
    return pd->posatmin;
}

/*!
    \property QQuickRangeModel1::positionAtMaximum
    \brief the maximum value that \l position can assume

    This property's default value is 0
*/

void QQuickRangeModel1::setPositionAtMaximum(qreal max)
{
    Q_PD(QQuickRangeModel1);
    setPositionRange(pd->posatmin, max);
}

qreal QQuickRangeModel1::positionAtMaximum() const
{
    Q_PD(const QQuickRangeModel1);
    return pd->posatmax;
}

/*!
    Returns a valid value, respecting the \l minimumValue,
    \l maximumValue and the \l stepSize properties.
    Such calculation is based on the parameter \a position (which is valid externally).
*/

qreal QQuickRangeModel1::valueForPosition(qreal position) const
{
    Q_PD(const QQuickRangeModel1);

    const qreal unconstrainedValue = pd->equivalentValue(position);
    return pd->publicValue(unconstrainedValue);
}

/*!
    \property QQuickRangeModel1::value
    \brief the current value of the model

    Represents a valid external value, based on the \l minimumValue,
    \l maximumValue and the \l stepSize properties.
    The user can set it internally with a value, that is not within the current range,
    since it can become valid if the user changes the range later.
*/

qreal QQuickRangeModel1::value() const
{
    Q_PD(const QQuickRangeModel1);

    // Return internal value but observe boundaries and
    // stepSize restrictions
    return pd->publicValue(pd->value);
}

void QQuickRangeModel1::setValue(qreal newValue)
{
    Q_PD(QQuickRangeModel1);

    if (qFuzzyCompare(newValue, pd->value))
        return;

    const qreal oldValue = value();
    const qreal oldPosition = position();

    // Update relative value and position
    pd->value = newValue;
    pd->pos = pd->equivalentPosition(pd->value);
    pd->emitValueAndPositionIfChanged(oldValue, oldPosition);
}

/*!
    \property QQuickRangeModel1::inverted
    \brief the model is inverted or not

    The model can be represented with an inverted behavior, e.g. when \l value assumes
    the maximum value (represented by \l maximumValue) the \l position will be at its
    minimum (represented by \l positionAtMinimum).
*/

void QQuickRangeModel1::setInverted(bool inverted)
{
    Q_PD(QQuickRangeModel1);
    if (inverted == bool(pd->inverted))
        return;

    pd->inverted = inverted;
    emit invertedChanged(pd->inverted);

    // After updating the internal value, the position property can change.
    setPosition(pd->equivalentPosition(pd->value));
}

bool QQuickRangeModel1::inverted() const
{
    Q_PD(const QQuickRangeModel1);
    return pd->inverted;
}

/*!
    Sets the \l value to \l minimumValue.
*/

void QQuickRangeModel1::toMinimum()
{
    Q_PD(const QQuickRangeModel1);
    setValue(pd->minimum);
}

/*!
    Sets the \l value to \l maximumValue.
*/

void QQuickRangeModel1::toMaximum()
{
    Q_PD(const QQuickRangeModel1);
    setValue(pd->maximum);
}

void QQuickRangeModel1::increaseSingleStep()
{
    Q_PD(const QQuickRangeModel1);
    if (qFuzzyIsNull(pd->stepSize))
        setValue(value() + (pd->maximum - pd->minimum)/10.0);
    else
        setValue(value() + pd->stepSize);
}

void QQuickRangeModel1::decreaseSingleStep()
{
    Q_PD(const QQuickRangeModel1);
    if (qFuzzyIsNull(pd->stepSize))
        setValue(value() - (pd->maximum - pd->minimum)/10.0);
    else
        setValue(value() - pd->stepSize);
}

QT_END_NAMESPACE
