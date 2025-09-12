#pragma once

#include <QSGGeometry>

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

    AbstractSequencePainter(const QRectF& viewport);

    virtual void append(std::vector<SequenceSample> samples) = 0;

    QSGGeometry& geometry() { return m_geometry; }

protected:
    const QRectF& m_viewport;
    QSGGeometry m_geometry;
};
} // namespace au::effects
