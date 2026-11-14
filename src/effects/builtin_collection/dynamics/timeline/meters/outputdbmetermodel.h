/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractdbmetermodel.h"

namespace au::effects {
class OutputDbMeterModel : public AbstractDbMeterModel
{
    Q_OBJECT

    Q_PROPERTY(bool isClipping READ isClipping WRITE setIsClipping NOTIFY isClippingChanged)

public:
    explicit OutputDbMeterModel(QObject* parent = nullptr);

    bool isClipping() const { return m_isClipping; }
    void setIsClipping(bool isClipping);

signals:
    void isClippingChanged();

private:
    bool m_isClipping = false;
    float latestValue() override;
};
} // namespace au::effects
