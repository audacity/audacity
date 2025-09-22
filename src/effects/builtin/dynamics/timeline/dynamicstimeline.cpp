/*
 * Audacity: A Digital Audio Editor
 */
#include "dynamicstimeline.h"
#include "dynamicscolors.h"
#include "painters/areasequencepainter.h"
#include "painters/linesequencepainter.h"

#include "global/log.h"

#include <QSGTransformNode>
#include <QTimer>
#include <QtQuick/qsgflatcolormaterial.h>

#include <algorithm>
#include <cassert>
#include <cmath>

namespace au::effects {
namespace {
constexpr auto dataPointPeriodMs = 10;
} // namespace

DynamicsTimeline::DynamicsTimeline(QQuickItem* parent)
    : QQuickItem{parent}
{
    setFlag(ItemHasContents, true);

    connect(this, &DynamicsTimeline::stopwatchTimeChanged, this, [this] { updateDrawerViewportX(); });
}

void DynamicsTimeline::componentComplete()
{
    QQuickItem::componentComplete();
    assert(m_dataPointRate > 0);
    assert(m_duration > 0);
    assert(m_dbMin < 0);
}

double DynamicsTimeline::stopwatchTime() const { return m_stopwatchTime; }

void DynamicsTimeline::setStopwatchTime(double stopwatchTime)
{
    if (m_stopwatchTime == stopwatchTime) {
        return;
    }

    m_stopwatchTime = stopwatchTime;
    emit stopwatchTimeChanged();
    update();
}

void DynamicsTimeline::clear()
{
    m_reset = true;
    update();
}

double DynamicsTimeline::dbMin() const { return m_dbMin; }

void DynamicsTimeline::setDbMin(double dbMin)
{
    if (m_dbMin == dbMin) {
        return;
    }
    m_dbMin = dbMin;

    emit dbMinChanged();
}

double DynamicsTimeline::duration() const { return m_duration; }

void DynamicsTimeline::setDuration(double duration)
{
    if (m_duration == duration) {
        return;
    }
    m_duration = duration;

    emit durationChanged();
}

double DynamicsTimeline::dataPointRate() const { return m_dataPointRate; }

void DynamicsTimeline::setDataPointRate(double rate)
{
    if (m_dataPointRate == rate) {
        return;
    }
    m_dataPointRate = rate;
    m_reset = true;
    emit dataPointRateChanged();
}

bool DynamicsTimeline::showInputDb() const
{
    return m_sequences[eInputDb].visible;
}

void DynamicsTimeline::setShowInputDb(bool show)
{
    if (m_sequences[eInputDb].visible == show) {
        return;
    }
    m_sequences[eInputDb].visible = show;
    update();
    emit showInputDbChanged();
}

bool DynamicsTimeline::showOutputDb() const
{
    return m_sequences[eOutputDb].visible;
}

void DynamicsTimeline::setShowOutputDb(bool show)
{
    if (m_sequences[eOutputDb].visible == show) {
        return;
    }
    m_sequences[eOutputDb].visible = m_sequences[eOutputDbLine].visible = show;
    update();
    emit showOutputDbChanged();
}

bool DynamicsTimeline::showCompressionDb() const
{
    return m_sequences[eCompressionDb].visible;
}

void DynamicsTimeline::setShowCompressionDb(bool show)
{
    if (m_sequences[eCompressionDb].visible == show) {
        return;
    }
    m_sequences[eCompressionDb].visible = show;
    update();
    emit showCompressionDbChanged();
}

namespace {
QSGGeometryNode* createGeometryNode(QColor color)
{
    auto node = new QSGGeometryNode();

    auto material = new QSGFlatColorMaterial();
    // color.setAlphaF(0.5);
    material->setColor(color);
    node->setMaterial(material);
    node->setFlag(QSGNode::OwnsMaterial);

    return node;
}
} // namespace

DynamicsTimeline::SequenceData
DynamicsTimeline::createSequenceData(const QColor& color, DrawerType drawerType, bool visible) const
{
    const auto maxNumSamples = static_cast<int>(std::ceil(m_duration * m_dataPointRate));
    std::unique_ptr<AbstractSequencePainter> drawer;
    if (drawerType == DrawerType::Area) {
        drawer = std::make_unique<AreaSequencePainter>(height(), m_drawerViewportX, maxNumSamples);
    } else {
        drawer = std::make_unique<LineSequencePainter>(m_drawerViewportX);
    }
    auto node = createGeometryNode(std::move(color));
    node->setGeometry(&drawer->geometry());
    return { visible, std::move(drawer), node };
}

void DynamicsTimeline::updateDrawerViewportX()
{
    m_drawerViewportX = timeToX(m_stopwatchTime - m_duration - m_timeDiff.load());
}

void DynamicsTimeline::resetSequences()
{
    QColor areaColor = DynamicsColors::timelineDataFillColor();
    areaColor.setAlphaF(0.5);
    QColor lineColor = DynamicsColors::timelineOutputDbLineColor();
    lineColor.setAlphaF(0.5);
    QColor compressionDbLineColor = DynamicsColors::timelineCompressionDbColor();
    compressionDbLineColor.setAlphaF(0.5);
    auto inputDb = createSequenceData(areaColor, DrawerType::Area, showInputDb());
    auto outputDb = createSequenceData(areaColor, DrawerType::Area, showOutputDb());
    auto outputDbLine = createSequenceData(lineColor, DrawerType::Line, showOutputDb());
    auto compressionDb = createSequenceData(compressionDbLineColor, DrawerType::Line, showCompressionDb());

    std::lock_guard<std::mutex> lock{ m_sampleMutex };
    m_sequences[eInputDb] = std::move(inputDb);
    m_sequences[eOutputDb] = std::move(outputDb);
    m_sequences[eOutputDbLine] = std::move(outputDbLine);
    m_sequences[eCompressionDb] = std::move(compressionDb);
    m_pendingXValues.clear();
    m_timeDiff.store(std::numeric_limits<float>::lowest());
}

QSGNode* DynamicsTimeline::updatePaintNode(QSGNode* oldNode,
                                           UpdatePaintNodeData*)
{
    QSGTransformNode* transformNode = nullptr;

    if (!oldNode || m_reset) {
        resetSequences();
        m_reset = false;
    }

    if (!oldNode) {
        transformNode = new QSGTransformNode();
    } else {
        transformNode = static_cast<QSGTransformNode*>(oldNode);
    }

    const auto someVisibilityChanged = std::any_of(
        m_sequences.begin(), m_sequences.end(), [](const SequenceData& seq) {
        return seq.visible != (seq.geometryNode->parent() != nullptr);
    });
    if (someVisibilityChanged) {
        // Brute-force visibility toggling to preserve order, since rendering
        // depends on it.
        transformNode->removeAllChildNodes();
        for (auto& seq : m_sequences) {
            if (seq.visible) {
                transformNode->appendChildNode(seq.geometryNode);
            }
        }
    }

    {
        std::lock_guard<std::mutex> lock{ m_sampleMutex };
        if (!m_pendingXValues.empty()) {
            std::vector<SequenceSample> drawerSamples;
            drawerSamples.reserve(m_pendingXValues.size());
            for (auto& seq : m_sequences) {
                for (auto i = 0; i < m_pendingXValues.size(); ++i) {
                    drawerSamples.push_back({ m_pendingXValues[i], seq.pendingYValues[i] });
                }
                seq.drawer->append(std::move(drawerSamples));
                seq.geometryNode->markDirty(QSGNode::DirtyGeometry);
                seq.pendingYValues.clear();
                drawerSamples.clear();
            }
            m_pendingXValues.clear();
        }
    }

    QMatrix4x4 matrix;
    matrix.translate(-m_drawerViewportX, 0);
    transformNode->setMatrix(matrix);
    transformNode->markDirty(QSGNode::DirtyMatrix);

    return transformNode;
}

double DynamicsTimeline::timeToX(double time) const
{
    // t = 0 means on the right of the graph.
    return width() * (1 + time / m_duration);
}

double DynamicsTimeline::xToTime(double x) const
{
    return (x / width() - 1) * m_duration;
}

double DynamicsTimeline::dbToY(double db) const
{
    return db / m_dbMin * this->height();
}

void DynamicsTimeline::onNewSamples(const QVariantList& variants)
{
    if (variants.empty()) {
        return;
    }

    QList<DynamicsSample> samples;
    samples.reserve(variants.size());
    for (const auto& variant : variants) {
        samples.push_back(variant.value<DynamicsSample>());
    }

    std::lock_guard<std::mutex> lock{ m_sampleMutex };
    // Delay by m_dataPointRate because two samples are needed for one vertical
    // strip, and otherwise we'd get ugly updates on the right side of the graph.

    const double newDiff = m_stopwatchTime - samples.front().time;
    if (m_timeDiff.load() < newDiff) {
        m_timeDiff.store(newDiff);
        updateDrawerViewportX();
    }

    for (const auto& sample : samples) {
        m_pendingXValues.push_back(timeToX(sample.time));
        m_sequences[eInputDb].pendingYValues.push_back(dbToY(sample.inputDb));
        m_sequences[eOutputDb].pendingYValues.push_back(dbToY(sample.outputDb));
        m_sequences[eOutputDbLine].pendingYValues.push_back(dbToY(sample.outputDb));
        m_sequences[eCompressionDb].pendingYValues.push_back(dbToY(sample.compressionDb));
    }
}
} // namespace au::effects
