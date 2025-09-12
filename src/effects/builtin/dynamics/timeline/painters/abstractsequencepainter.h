#pragma once

#include <QSGGeometry>

#include <atomic>

namespace au::effects {
struct SequenceSample {
    SequenceSample(double x, double y)
        : x{x}, y{y} {}
    const double x;
    const double y;
};

class AbstractSequencePainter
{
public:
    virtual ~AbstractSequencePainter() = default;

    AbstractSequencePainter(const std::atomic<double>& viewportX);

    virtual void append(std::vector<SequenceSample> samples) = 0;

    QSGGeometry& geometry() { return m_geometry; }

protected:
    const std::atomic<double>& m_viewportX;
    QSGGeometry m_geometry;
};
} // namespace au::effects
