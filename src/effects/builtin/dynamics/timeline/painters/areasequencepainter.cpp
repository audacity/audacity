/*
 * Audacity: A Digital Audio Editor
 */
#include "areasequencepainter.h"

#include "global/log.h"

namespace au::effects {
namespace {
constexpr auto getNumVertices(int numSamples) { return 2 * numSamples; }
constexpr auto getNumSamples(int numVertices) { return numVertices / 2; }

static_assert(getNumSamples(getNumVertices(100)) == 100);

constexpr auto getVertexIndex(int sampleIndex)
{
    return getNumVertices(sampleIndex);
}

static_assert(getVertexIndex(0) == 0);
static_assert(getVertexIndex(1) == 2);
static_assert(getVertexIndex(2) == 4);
} // namespace

AreaSequencePainter::AreaSequencePainter(double viewportHeight, const std::atomic<double>& viewportX, int maxNumSamples)
    : AbstractSequencePainter{viewportX}, m_viewportHeight{viewportHeight}
{
    m_buffer.reserve(getNumVertices(maxNumSamples + 10));
    m_geometry.setDrawingMode(QSGGeometry::DrawTriangleStrip);
    m_geometry.setLineWidth(1);
}

void AreaSequencePainter::append(std::vector<SequenceSample> newSamples)
{
    const auto numVerticesNow = m_geometry.vertexCount();
    const auto numSamplesNow = getNumSamples(numVerticesNow);
    const auto numSamplesAfterDiscard = numSamplesNow - numSamplesToDiscard();
    const auto numVerticesAfterDiscard = getNumVertices(numSamplesAfterDiscard);

    m_buffer.resize(numVerticesAfterDiscard);
    QSGGeometry::Point2D* const vertices = m_geometry.vertexDataAsPoint2D();
    const auto toDiscard = numVerticesNow - numVerticesAfterDiscard;
    std::copy(vertices + toDiscard, vertices + numVerticesNow, m_buffer.begin());

    const auto newNumSamples
        =numSamplesAfterDiscard + static_cast<int>(newSamples.size());
    const auto newNumVertices = getNumVertices(newNumSamples);
    m_geometry.allocate(newNumVertices);

    QSGGeometry::Point2D* vertex = m_geometry.vertexDataAsPoint2D();
    std::copy(m_buffer.begin(), m_buffer.end(), vertex);
    vertex += m_buffer.size();
    for (const auto& sample : newSamples) {
        (vertex++)->set(sample.x, std::min(sample.y, m_viewportHeight));
        (vertex++)->set(sample.x, m_viewportHeight);
    }
}

int AreaSequencePainter::numSamplesToDiscard() const
{
    const auto numSamples = getNumSamples(m_geometry.vertexCount());
    const QSGGeometry::Point2D* const vertices = m_geometry.vertexDataAsPoint2D();
    for (auto i = 0; i < numSamples; ++i) {
        const auto vertexIndex = getVertexIndex(i);
        if (vertices[vertexIndex].x < m_viewportX.load()) {
            continue;
        } else {
            return std::max(0, i - 1);
        }
    }
    return numSamples;
}
} // namespace au::effects
