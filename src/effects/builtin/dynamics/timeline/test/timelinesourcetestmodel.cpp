/*
 * Audacity: A Digital Audio Editor
 */
#include "timelinesourcetestmodel.h"
#include "dynamicstimelinetypes.h"

#include <QLineSeries>
#include <QSGGeometryNode>
#include <QTimer>
#include <QtQuick/qsgflatcolormaterial.h>

#include <algorithm>
#include <array>
#include <cmath>

namespace au::effects {
namespace {
constexpr auto deliveryIntervalMs = 250;
constexpr auto dataPointPeriodMs = 50;
} // namespace

TimelineSourceTestModel::TimelineSourceTestModel(QObject* parent)
    : QObject(parent) {}

void TimelineSourceTestModel::init()
{
    m_deliveryTimer = new QTimer(this);
    connect(m_deliveryTimer, &QTimer::timeout, this, [this] { addDataPoints(); });
    m_deliveryTimer->start(deliveryIntervalMs);
}

double TimelineSourceTestModel::dataPointRate() const
{
    return 1000.0 / dataPointPeriodMs;
}

double TimelineSourceTestModel::latency() const
{
    return deliveryIntervalMs / 1000.0;
}

void TimelineSourceTestModel::addDataPoints()
{
    static auto i = 0;

    QVariantList samples;
    constexpr auto pointsPerDelivery = deliveryIntervalMs / dataPointPeriodMs;
    samples.reserve(pointsPerDelivery);

    for (auto j = 0; j < pointsPerDelivery; ++j) {
        constexpr std::array<double, 4> period{ 0.0, -15.0, -10.0, -20.0 };
        const double inputDb = period[i++ % period.size()];
        const double outputDb = inputDb - (rand() % 100 / 10.0);
        const double compressionDb = -(rand() % 100 / 20.0);
        const double time = m_dataPointCount++ *dataPointPeriodMs / 1000.0;
        samples.append(QVariant::fromValue(DynamicsSample { time, inputDb, outputDb, compressionDb }));
    }

    emit newSamples(samples);
}

void TimelineSourceTestModel::setInstanceId(int id)
{
    if (m_instanceId == id) {
        return;
    }
    m_instanceId = id;
    emit instanceIdChanged();
}
} // namespace au::effects
