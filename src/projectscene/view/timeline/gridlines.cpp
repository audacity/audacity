/*
* Audacity: A Digital Audio Editor
*/

#include "gridlines.h"

#include <algorithm>
#include <cmath>

#include <QSGFlatColorMaterial>
#include <QSGGeometry>
#include <QSGGeometryNode>

using namespace au::projectscene;

namespace {
constexpr int MINOR_LINE_ALPHA = static_cast<int>(0.06 * 256);
constexpr int MAJOR_LINE_ALPHA = static_cast<int>(0.1 * 256);
constexpr int ZEBRA_ALPHA = static_cast<int>(0.02 * 256);

QSGGeometryNode* makeNode()
{
    auto* node = new QSGGeometryNode();

    auto* geometry = new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), 0);
    geometry->setDrawingMode(QSGGeometry::DrawTriangles);
    node->setGeometry(geometry);
    node->setFlag(QSGNode::OwnsGeometry);

    node->setMaterial(new QSGFlatColorMaterial());
    node->setFlag(QSGNode::OwnsMaterial);

    return node;
}
}

GridLines::GridLines(QQuickItem* parent)
    : QQuickItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setFlag(ItemHasContents, true);

    uiconfiguration()->currentThemeChanged().onNotify(this, [this]() { update(); });
}

TimelineRuler* GridLines::timelineRuler() const
{
    return m_timelineRuler;
}

void GridLines::setTimelineRuler(TimelineRuler* newTimelineRuler)
{
    if (m_timelineRuler == newTimelineRuler) {
        return;
    }

    if (m_timelineRuler) {
        disconnect(m_timelineRuler, nullptr, this, nullptr);
    }

    m_timelineRuler = newTimelineRuler;
    update();

    if (m_timelineRuler) {
        connect(m_timelineRuler, &TimelineRuler::ticksChanged, this, [this]() { update(); });
    }

    emit timelineRulerChanged();
}

void GridLines::geometryChange(const QRectF& newGeometry, const QRectF& oldGeometry)
{
    QQuickItem::geometryChange(newGeometry, oldGeometry);

    if (newGeometry.size() != oldGeometry.size()) {
        update();
    }
}

QSGNode* GridLines::updatePaintNode(QSGNode* oldNode, UpdatePaintNodeData*)
{
    enum Layer {
        Background, ZebraHighlighting, MinorLines, MajorLines, LayerCount
    };

    QSGNode* root = oldNode;
    if (!root) {
        root = new QSGNode();
        for (int i = 0; i < LayerCount; ++i) {
            root->appendChildNode(makeNode());
        }
    }

    //! NOTE: Asked for here, not taken from what the ruler painted: that would come a frame late
    const Ticks ticks = m_timelineRuler ? m_timelineRuler->ticks() : Ticks {};

    const auto& themeValues = uiconfiguration()->currentTheme().values;
    const QColor canvasColor = themeValues.value(muse::ui::BACKGROUND_QUARTERNARY_COLOR).toString();
    QColor minorLineColor = themeValues.value(muse::ui::WHITE_COLOR).toString();
    QColor majorLineColor = minorLineColor;
    QColor highlightColor = minorLineColor;
    minorLineColor.setAlpha(MINOR_LINE_ALPHA);
    majorLineColor.setAlpha(MAJOR_LINE_ALPHA);
    highlightColor.setAlpha(ZEBRA_ALPHA);

    Rects minorLines;
    // the left border
    Rects majorLines { QRectF(0.0, 0.0, 1.0, height()) };
    collectGridLines(ticks, minorLines, majorLines);

    Rects highlights;
    if (projectSceneUiState()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES) {
        collectZebraHighlighting(ticks, highlights);
    }

    auto layer = [root](Layer index) { return static_cast<QSGGeometryNode*>(root->childAtIndex(index)); };
    fillNode(layer(Background), { boundingRect() }, canvasColor);
    fillNode(layer(ZebraHighlighting), highlights, highlightColor);
    fillNode(layer(MinorLines), minorLines, minorLineColor);
    fillNode(layer(MajorLines), majorLines, majorLineColor);

    return root;
}

void GridLines::fillNode(QSGGeometryNode* node, const Rects& rects, const QColor& color)
{
    QSGGeometry* geometry = node->geometry();
    geometry->allocate(static_cast<int>(rects.size()) * 6);

    QSGGeometry::Point2D* vertices = geometry->vertexDataAsPoint2D();
    for (const QRectF& rect : rects) {
        const float left = static_cast<float>(rect.left());
        const float right = static_cast<float>(rect.right());
        const float top = static_cast<float>(rect.top());
        const float bottom = static_cast<float>(rect.bottom());

        vertices[0].set(left, top);
        vertices[1].set(right, top);
        vertices[2].set(left, bottom);
        vertices[3].set(left, bottom);
        vertices[4].set(right, top);
        vertices[5].set(right, bottom);
        vertices += 6;
    }

    auto* material = static_cast<QSGFlatColorMaterial*>(node->material());
    if (material->color() != color) {
        material->setColor(color);
        node->markDirty(QSGNode::DirtyMaterial);
    }
    node->markDirty(QSGNode::DirtyGeometry);
}

void GridLines::collectGridLines(const Ticks& ticks, Rects& minorLines, Rects& majorLines) const
{
    const qreal h = height();
    const qreal w = width();

    //! NOTE: The ticks are on the grid of device pixels, and so are the lines, a pixel wide and centered on them
    const double ratio = std::max(m_timelineRuler->timelineContext()->devicePixelRatio(), 1.0);
    const double lineWidth = std::max(1.0, std::round(ratio)) / ratio;

    for (const auto& tick : ticks) {
        const double x = std::round((tick.line.p1().x() - lineWidth / 2) * ratio) / ratio;
        if (x + lineWidth <= 0.0 || x >= w) {
            continue;
        }

        Rects& lines = tick.tickType == TickType::MAJOR ? majorLines : minorLines;
        lines.emplace_back(x, 0.0, lineWidth, h);
    }
}

void GridLines::collectZebraHighlighting(const Ticks& ticks, Rects& highlights) const
{
    if (ticks.isEmpty()) {
        return;
    }

    const qreal h = height();
    const qreal w = width();

    int BPM = m_timelineRuler->timelineContext()->BPM();
    double SPB = static_cast<double>(60) / BPM;
    IntervalInfo intervalInfo = m_timelineRuler->intervalInfo();

    // highlight only when displaying at zoom between 1/4 and 1/32 ticks
    if (intervalInfo.minorMinor >= 2 * SPB || intervalInfo.minorMinor <= (SPB / 16)) {
        return;
    }

    double highlightStart = 0.0;
    bool pendingDraw = false;
    const double tolerance = 0.01; // Tolerance for floating-point comparison

    auto addHighlight = [&](double from, double to) {
        from = std::max(from, 0.0);
        to = std::min(to, w);
        if (to > from) {
            highlights.emplace_back(QPointF(from, 0), QPointF(to, h));
        }
    };

    for (const auto& tick : ticks) {
        double spbRemainder = std::abs(std::remainder(tick.timeValue, SPB));
        double doubledSpbRemainder = std::abs(std::remainder(tick.timeValue, SPB * 2));
        if (spbRemainder < tolerance && doubledSpbRemainder < tolerance) {
            highlightStart = tick.line.p1().x();
            pendingDraw = true;
        } else if (spbRemainder < tolerance && doubledSpbRemainder >= tolerance) {
            addHighlight(highlightStart, tick.line.p1().x());
            pendingDraw = false;
        }
    }

    if (pendingDraw) {
        addHighlight(highlightStart, w);
    }
}
