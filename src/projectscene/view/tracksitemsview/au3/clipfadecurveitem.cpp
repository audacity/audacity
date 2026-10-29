/*
 * Audacity: A Digital Audio Editor
 */
#include "clipfadecurveitem.h"

#include <QPainter>
#include <QPen>
#include <QPolygonF>

#include <algorithm>
#include <cmath>

#include "au3-clip-processing/ClipFades.h"

using namespace au::projectscene;

namespace {
constexpr qreal dashLength = 4;

const QColor lineColor = QColor::fromRgbF(0, 0, 0, 0.5);
const QColor shadingColor = QColor::fromRgbF(0, 0, 0, 0.2);
const QColor highlightColor = Qt::white;

::FadeType au3FadeType(FadeTypes::Type type)
{
    switch (type) {
    case FadeTypes::Type::LOGARITHMIC:
        return ::FadeType::Logarithmic;
    case FadeTypes::Type::LINEAR:
    default:
        return ::FadeType::Linear;
    }
}
}

ClipFadeCurveItem::ClipFadeCurveItem(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setAntialiasing(true);
}

template<typename T>
void ClipFadeCurveItem::set(T& member, const T& value)
{
    if (member == value) {
        return;
    }
    member = value;
    update();
}

void ClipFadeCurveItem::setIsFadeIn(bool isFadeIn) { set(m_isFadeIn, isFadeIn); }
void ClipFadeCurveItem::setFadeLeft(qreal left) { set(m_fadeLeft, left); }
void ClipFadeCurveItem::setFadeWidth(qreal width) { set(m_fadeWidth, width); }
void ClipFadeCurveItem::setFadeType(FadeTypes::Type type) { set(m_fadeType, type); }
void ClipFadeCurveItem::setDrawingTop(qreal top) { set(m_drawingTop, top); }
void ClipFadeCurveItem::setHighlighted(bool highlighted) { set(m_highlighted, highlighted); }

void ClipFadeCurveItem::paint(QPainter* painter)
{
    if (!painter || m_drawingTop >= height()) {
        return;
    }
    painter->setRenderHint(QPainter::Antialiasing, true);
    painter->setClipRect(QRectF(0, m_drawingTop, width(), height() - m_drawingTop));

    if (m_fadeWidth >= 1) {
        drawBoundaryLine(*painter);
    }
    drawCurve(*painter);
}

void ClipFadeCurveItem::drawBoundaryLine(QPainter& painter) const
{
    // Drawn just inside the fade
    const qreal x = m_isFadeIn ? m_fadeLeft + m_fadeWidth - 0.5 : m_fadeLeft + 0.5;
    if (x < 0 || x > width()) {
        return;
    }

    const QLineF line(x, m_drawingTop, x, height());

    QPen pen(lineColor, 1, Qt::CustomDashLine, Qt::FlatCap);
    pen.setDashPattern({ dashLength, dashLength });

    // Fills the gaps between the dashes
    if (m_highlighted) {
        QPen highlightPen = pen;
        highlightPen.setColor(highlightColor);
        highlightPen.setDashOffset(dashLength);
        painter.setPen(highlightPen);
        painter.drawLine(line);
    }

    painter.setPen(pen);
    painter.drawLine(line);
}

void ClipFadeCurveItem::drawCurve(QPainter& painter) const
{
    // Only the part of the fade within this item: when zoomed in, drawing all of it would mean huge coordinates
    const qreal visibleLeft = std::max<qreal>(m_fadeLeft, 0);
    const qreal visibleRight = std::min<qreal>(m_fadeLeft + m_fadeWidth, width());
    const qreal visibleWidth = visibleRight - visibleLeft;
    if (m_fadeWidth <= 0 || visibleWidth <= 0) {
        return;
    }

    const auto au3Type = au3FadeType(m_fadeType);
    const int segmentCount = std::max(1, static_cast<int>(std::ceil(visibleWidth)));

    QPolygonF line;
    line.reserve(segmentCount + 1);
    for (int i = 0; i <= segmentCount; ++i) {
        const qreal x = visibleLeft + i * visibleWidth / segmentCount;
        const qreal position = (x - m_fadeLeft) / m_fadeWidth;
        // From the silent end of the fade
        const qreal gain = ClipFades::fadeGain(au3Type, m_isFadeIn ? position : 1 - position);
        line << QPointF(x, height() * (1 - gain));
    }

    // The faded-out part, above the line: the line closed by the top of the item
    QPolygonF shading;
    shading.reserve(line.size() + 2);
    shading << QPointF(visibleLeft, 0) << line << QPointF(visibleRight, 0);

    painter.setPen(Qt::NoPen);
    painter.setBrush(shadingColor);
    painter.drawPolygon(shading);

    painter.setBrush(Qt::NoBrush);
    painter.setPen(QPen(lineColor, 1, Qt::SolidLine, Qt::FlatCap));
    painter.drawPolyline(line);

    if (m_highlighted) {
        painter.setPen(QPen(highlightColor, 1.5, Qt::SolidLine, Qt::FlatCap));
        painter.drawPolyline(line);
    }
}
