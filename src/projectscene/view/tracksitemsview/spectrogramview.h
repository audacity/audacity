/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QQuickPaintedItem>

namespace au::projectscene {
class SpectrogramView : public QQuickPaintedItem
{
    Q_OBJECT
public:
    SpectrogramView(QQuickItem* parent = nullptr);
    ~SpectrogramView() override;

    void paint(QPainter* painter) override;
};
}
