#include "abstractdynamicsmetermodel.h"

#include <cmath>

namespace au::effects {
AbstractDynamicsMeterModel::AbstractDynamicsMeterModel(QObject* parent)
    : QObject{parent}
{
    connect(&m_timer, &QTimer::timeout, this, [this] { onTick(); });
    m_timer.setInterval(1000);
    m_timer.start();
}

void AbstractDynamicsMeterModel::onTick()
{
    m_currentMax = rand() % 100 / -10.0;
    m_globalMax = m_currentMax + (rand() % 100 / 3.0)
                  * (direction() == Direction::Upwards ? -1 : 1);
    m_fiveSecMax = (m_currentMax + m_globalMax) / 2.0;

    emit valueChanged();
}

double AbstractDynamicsMeterModel::currentMax() const { return m_currentMax; }

double AbstractDynamicsMeterModel::globalMax() const { return m_globalMax; }

double AbstractDynamicsMeterModel::fiveSecMax() const { return m_fiveSecMax; }
} // namespace au::effects
