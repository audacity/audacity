/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QColor>
#include <QQuickPaintedItem>

#include "types/projectscenetypes.h"

class QPainter;

namespace au::projectscene {
//! Paints a fade of a clip: its curve, the shading of the faded-out part and the boundary line
class ClipFadeCurveItem : public QQuickPaintedItem
{
    Q_OBJECT

    Q_PROPERTY(bool isFadeIn READ isFadeIn WRITE setIsFadeIn FINAL)
    //! Where the fade starts, in this item's coordinates: possibly far outside it, which only needs to span the visible part of the fade
    Q_PROPERTY(qreal fadeLeft READ fadeLeft WRITE setFadeLeft FINAL)
    Q_PROPERTY(qreal fadeWidth READ fadeWidth WRITE setFadeWidth FINAL)
    Q_PROPERTY(FadeTypes::Type fadeType READ fadeType WRITE setFadeType FINAL)
    //! Above it, the fade isn't drawn
    Q_PROPERTY(qreal drawingTop READ drawingTop WRITE setDrawingTop FINAL)
    //! E.g. while the fade is dragged
    Q_PROPERTY(bool highlighted READ highlighted WRITE setHighlighted FINAL)

public:
    explicit ClipFadeCurveItem(QQuickItem* parent = nullptr);

    void paint(QPainter* painter) override;

    bool isFadeIn() const { return m_isFadeIn; }
    void setIsFadeIn(bool isFadeIn);
    qreal fadeLeft() const { return m_fadeLeft; }
    void setFadeLeft(qreal left);
    qreal fadeWidth() const { return m_fadeWidth; }
    void setFadeWidth(qreal width);
    FadeTypes::Type fadeType() const { return m_fadeType; }
    void setFadeType(FadeTypes::Type type);
    qreal drawingTop() const { return m_drawingTop; }
    void setDrawingTop(qreal top);
    bool highlighted() const { return m_highlighted; }
    void setHighlighted(bool highlighted);

private:
    template<typename T>
    void set(T& member, const T& value);

    void drawBoundaryLine(QPainter& painter) const;
    void drawCurve(QPainter& painter) const;

    bool m_isFadeIn = true;
    qreal m_fadeLeft = 0;
    qreal m_fadeWidth = 0;
    FadeTypes::Type m_fadeType = FadeTypes::Type::LINEAR;
    qreal m_drawingTop = 0;
    bool m_highlighted = false;
};
}
