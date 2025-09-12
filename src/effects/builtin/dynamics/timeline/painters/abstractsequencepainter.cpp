#include "abstractsequencepainter.h"

namespace au::effects {
AbstractSequencePainter::AbstractSequencePainter(const std::atomic<double>& viewportX)
    : m_viewportX{viewportX},
    m_geometry{QSGGeometry::defaultAttributes_Point2D(), 0} {}
} // namespace au::effects
