/*
 * Audacity: A Digital Audio Editor
 */
#include "filtercurvemodel.h"

#include <algorithm>

#include "filtercurveeq.h"

#include "au3-builtin-effects/EqualizationCurvesList.h"
#include "au3-builtin-effects/EqualizationFilter.h"
#include "au3-mixer/Envelope.h"

namespace au::effects {
FilterCurveModel::FilterCurveModel(QObject* parent, FilterCurveEq& eq)
    : QObject(parent), m_eq(eq)
{
}

void FilterCurveModel::reload()
{
    auto& parameters = m_eq.mCurvesList.mParameters;
    parameters.mDrawMode = true;

    rebuildFromEnvelope();
    emit pointsChanged();
}

QVector<QPointF> FilterCurveModel::points() const
{
    return m_points;
}

double FilterCurveModel::defaultValue() const
{
    return 0.0;
}

void FilterCurveModel::setPoint(int index, double x, double y, bool completed)
{
    if (index < 0 || index >= m_points.size()) {
        return;
    }
    beginDragIfNeeded();

    m_points[index] = QPointF(x, y);
    syncToEnvelope();
    emit pointsChanged();

    commitIfCompleted(completed);
}

void FilterCurveModel::addPoint(double x, double y, bool completed)
{
    beginDragIfNeeded();

    m_points.append(QPointF(x, y));
    syncToEnvelope();
    emit pointsChanged();

    commitIfCompleted(completed);
}

void FilterCurveModel::removePoint(int index, bool completed)
{
    if (index < 0 || index >= m_points.size()) {
        return;
    }
    beginDragIfNeeded();

    m_points.remove(index);
    syncToEnvelope();
    emit pointsChanged();

    commitIfCompleted(completed);
}

void FilterCurveModel::cancelDrag()
{
    if (!m_dragSnapshot) {
        return;
    }
    m_points = *m_dragSnapshot;
    m_dragSnapshot.reset();
    syncToEnvelope();
    emit pointsChanged();
}

void FilterCurveModel::flatten()
{
    auto& parameters = m_eq.mCurvesList.mParameters;
    parameters.mLogEnvelope.Flatten(0.0);
    parameters.mLogEnvelope.SetTrackLen(1.0);
    parameters.mLinEnvelope.Flatten(0.0);
    parameters.mLinEnvelope.SetTrackLen(1.0);

    m_points.clear();
    m_eq.mCurvesList.EnvelopeUpdated();
    emit pointsChanged();
}

void FilterCurveModel::invert()
{
    for (auto& p : m_points) {
        p.setY(-p.y());
    }
    syncToEnvelope();
    m_eq.mCurvesList.EnvelopeUpdated();
    emit pointsChanged();
}

void FilterCurveModel::rebuildFromEnvelope()
{
    m_points.clear();

    const auto& env = m_eq.mCurvesList.mParameters.ChooseEnvelope();
    const size_t n = env.GetNumberOfPoints();
    if (n == 0) {
        return;
    }

    std::vector<double> when(n);
    std::vector<double> value(n);
    env.GetPoints(when.data(), value.data(), static_cast<int>(n));

    m_points.reserve(static_cast<int>(n));
    for (size_t i = 0; i < n; ++i) {
        m_points.append(QPointF(when[i], value[i]));
    }
}

void FilterCurveModel::syncToEnvelope()
{
    auto& env = m_eq.mCurvesList.mParameters.ChooseEnvelope();
    while (env.GetNumberOfPoints() > 0) {
        env.Delete(0);
    }
    auto sorted = m_points;
    std::sort(sorted.begin(), sorted.end(),
              [](const QPointF& a, const QPointF& b) { return a.x() < b.x(); });
    for (const auto& p : sorted) {
        env.Insert(p.x(), p.y());
    }
}

void FilterCurveModel::beginDragIfNeeded()
{
    if (!m_dragSnapshot) {
        m_dragSnapshot = m_points;
    }
}

void FilterCurveModel::commitIfCompleted(bool completed)
{
    if (!completed) {
        return;
    }
    m_eq.mCurvesList.EnvelopeUpdated();
    m_dragSnapshot.reset();
}
}
