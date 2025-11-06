/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractclipview.h"

namespace au::projectscene {
class SpectrogramView : public AbstractClipView
{
    Q_OBJECT

public:
    SpectrogramView(QQuickItem* parent = nullptr);
    ~SpectrogramView() override = default;

    void setTimelineContext(TimelineContext* newContext) override;

    void paint(QPainter* painter) override;
};
}
