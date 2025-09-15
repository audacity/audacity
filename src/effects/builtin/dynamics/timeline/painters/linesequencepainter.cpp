/*
 * Audacity: A Digital Audio Editor
 */
#include "linesequencepainter.h"

namespace au::effects {
LineSequencePainter::LineSequencePainter(const double& viewportX)
    : AbstractSequencePainter{viewportX}
{
    m_geometry.setDrawingMode(QSGGeometry::DrawLineStrip);
    m_geometry.setLineWidth(1);
}

void LineSequencePainter::append(std::vector<SequenceSample> newSamples)
{
    const auto numSamplesNow = m_geometry.vertexCount();
    const auto toDiscard = numSamplesToDiscard();
    const auto numSamplesAfterDiscard = numSamplesNow - toDiscard;
    m_buffer.resize(numSamplesAfterDiscard);
    QSGGeometry::Point2D* const vertices = m_geometry.vertexDataAsPoint2D();
    std::copy(vertices + toDiscard, vertices + numSamplesNow, m_buffer.begin());

    const auto newNumSamples
        =numSamplesAfterDiscard + static_cast<int>(newSamples.size());
    m_geometry.allocate(newNumSamples);
    QSGGeometry::Point2D* vertex = m_geometry.vertexDataAsPoint2D();
    std::copy(m_buffer.begin(), m_buffer.end(), vertex);
    vertex += m_buffer.size();
    for (const auto& sample : newSamples) {
        (vertex++)->set(sample.x, sample.y);
    }
}

int LineSequencePainter::numSamplesToDiscard() const
{
    const auto numSamples = m_geometry.vertexCount();
    const QSGGeometry::Point2D* const vertices = m_geometry.vertexDataAsPoint2D();
    for (auto i = 0; i < numSamples; ++i) {
        if (vertices[i].x < m_viewportX.load()) {
            continue;
        } else {
            return std::max(0, -1);
        }
    }
    return numSamples;
}
} // namespace au::effects
