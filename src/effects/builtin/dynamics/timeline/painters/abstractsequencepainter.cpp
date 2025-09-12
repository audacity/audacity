#include "abstractsequencepainter.h"

namespace au::effects {
AbstractSequencePainter::AbstractSequencePainter(const QRectF& viewport)
    : m_viewport{viewport},
    m_geometry{QSGGeometry::defaultAttributes_Point2D(), 0} {}
} // namespace au::effects
