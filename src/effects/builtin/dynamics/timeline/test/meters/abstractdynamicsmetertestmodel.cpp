#include "abstractdynamicsmetertestmodel.h"

#include <cmath>

namespace au::effects {
AbstractDynamicsMeterTestModel::AbstractDynamicsMeterTestModel(QObject* parent)
    : QObject{parent}
{
    connect(&m_timer, &QTimer::timeout, this, [this] { onTick(); });
    m_timer.setInterval(1000);
    m_timer.start();
}

void AbstractDynamicsMeterTestModel::onTick()
{
    m_currentMax = rand() % 100 / -10.0;
    m_globalMax = m_currentMax + (rand() % 100 / 3.0)
                  * (direction() == Direction::Upwards ? -1 : 1);
    m_fiveSecMax = (m_currentMax + m_globalMax) / 2.0;

    emit valueChanged();
}

double AbstractDynamicsMeterTestModel::currentMax() const { return m_currentMax; }

double AbstractDynamicsMeterTestModel::globalMax() const { return m_globalMax; }

double AbstractDynamicsMeterTestModel::fiveSecMax() const { return m_fiveSecMax; }

int AbstractDynamicsMeterTestModel::instanceId() const { return m_instanceId; }

void AbstractDynamicsMeterTestModel::setInstanceId(int id)
{
    if (m_instanceId == id) {
        return;
    }
    m_instanceId = id;
    emit instanceIdChanged();
}
} // namespace au::effects
