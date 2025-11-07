/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramview.h"

namespace au::projectscene {
SpectrogramView::SpectrogramView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
}

SpectrogramView::~SpectrogramView() = default;

void SpectrogramView::paint(QPainter* painter)
{
    painter->fillRect(boundingRect(), Qt::gray);
}
}
