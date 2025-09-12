#include "timelinesourcemodel.h"

#include <QLineSeries>
#include <QSGGeometryNode>
#include <QTimer>
#include <QtQuick/qsgflatcolormaterial.h>

#include <algorithm>
#include <array>
#include <cmath>

namespace au::effects {
namespace {
constexpr auto samplePeriodMs = 250;
} // namespace

TimelineSourceModel::TimelineSourceModel(QObject* parent)
    : QObject(parent) {}

void TimelineSourceModel::init()
{
    m_sampleTimer = new QTimer(this);
    connect(m_sampleTimer, &QTimer::timeout, this, [this] { addDataPoint(); });
    m_sampleTimer->start(samplePeriodMs);

    m_clippingTimer = new QTimer(this);
    connect(m_clippingTimer, &QTimer::timeout, this, [this] {
        m_isClipping = !m_isClipping;
        emit isClippingChanged();
    });
    m_clippingTimer->start(3000);
}

double TimelineSourceModel::samplePeriod() const
{
    return samplePeriodMs / 1000.0;
}

void TimelineSourceModel::addDataPoint()
{
    static auto i = 0;
    constexpr std::array<double, 4> period{ 0.0, -15.0, -10.0, -20.0 };
    const double inputDb = period[i++ % period.size()];
    const double outputDb = inputDb - (rand() % 100 / 10.0);
    const double compressionDb = -(rand() % 100 / 20.0);

    emit newSample(inputDb, outputDb, compressionDb);
}

void TimelineSourceModel::setIsClipping(bool clipping)
{
    if (m_isClipping == clipping) {
        return;
    }
    m_isClipping = clipping;
    emit isClippingChanged();
}
} // namespace au::effects
