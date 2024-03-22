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

#ifndef QQUICKRANGEMODEL_P_P_H
#define QQUICKRANGEMODEL_P_P_H

//
//  W A R N I N G
//  -------------
//
// This file is not part of the Qt Components API.  It exists purely as an
// implementation detail.  This header file may change from version to
// version without notice, or even be removed.
//
// We mean it.
//

#include "qquickrangemodel_p.h"

QT_BEGIN_NAMESPACE

class QQuickRangeModel1Private
{
    Q_DECLARE_PUBLIC(QQuickRangeModel1)
public:
    QQuickRangeModel1Private(QQuickRangeModel1 *qq);
    virtual ~QQuickRangeModel1Private();

    void init();

    qreal posatmin, posatmax;
    qreal minimum, maximum, stepSize, pos, value;

    uint inverted : 1;

    QQuickRangeModel1 *q_ptr;
    bool isComplete;
    bool positionChanged;
    bool valueChanged;

    inline qreal effectivePosAtMin() const {
        return inverted ? posatmax : posatmin;
    }

    inline qreal effectivePosAtMax() const {
        return inverted ? posatmin : posatmax;
    }

    inline qreal equivalentPosition(qreal aValue) const {
        // Return absolute position from absolute value
        const qreal valueRange = maximum - minimum;
        if (valueRange == 0)
            return effectivePosAtMin();

        const qreal scale = (effectivePosAtMax() - effectivePosAtMin()) / valueRange;
        return (aValue - minimum) * scale + effectivePosAtMin();
    }

    inline qreal equivalentValue(qreal aPos) const {
        // Return absolute value from absolute position
        const qreal posRange = effectivePosAtMax() - effectivePosAtMin();
        if (posRange == 0)
            return minimum;

        const qreal scale = (maximum - minimum) / posRange;
        // Avoid perverse rounding glitches when at an end:
        const qreal mid = (effectivePosAtMax() + effectivePosAtMin()) * 0.5;
        if (aPos < mid)
            return (aPos - effectivePosAtMin()) * scale + minimum;
        return maximum - scale * (effectivePosAtMax() - aPos);
    }

    qreal publicPosition(qreal position) const;
    qreal publicValue(qreal value) const;
    void emitValueAndPositionIfChanged(const qreal oldValue, const qreal oldPosition);
};

QT_END_NAMESPACE

#endif // QQUICKRANGEMODEL_P_P_H
