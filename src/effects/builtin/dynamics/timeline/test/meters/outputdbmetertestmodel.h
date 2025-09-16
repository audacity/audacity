/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractdynamicsmetertestmodel.h"

#include <QTimer>

namespace au::effects {
class OutputDbMeterTestModel : public AbstractDynamicsMeterTestModel
{
    Q_OBJECT

    Q_PROPERTY(bool isClipping READ isClipping WRITE setIsClipping NOTIFY isClippingChanged)

public:
    explicit OutputDbMeterTestModel(QObject* = nullptr)
    {
        m_clippingTimer = new QTimer(this);
        connect(m_clippingTimer, &QTimer::timeout, this, [this] {
            m_isClipping = !m_isClipping;
            emit isClippingChanged();
        });
        m_clippingTimer->start(3000);
    }

    Direction direction() const override { return Direction::Upwards; }

    bool isClipping() const { return m_isClipping; }
    void setIsClipping(bool isClipping)
    {
        if (m_isClipping == isClipping) {
            return;
        }

        m_isClipping = isClipping;
        emit isClippingChanged();
    }

signals:
    void isClippingChanged();

private:
    bool m_isClipping = false;
    QTimer* m_clippingTimer = nullptr;
};
} // namespace au::effects
