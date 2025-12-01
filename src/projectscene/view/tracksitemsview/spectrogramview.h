/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogrampainter.h"
#include "abstractclipview.h"

#include "context/iglobalcontext.h"

namespace au::projectscene {
class SpectrogramView : public AbstractClipView
{
    Q_OBJECT
    Q_PROPERTY(int timelineIndentWidth READ timelineIndentWidth WRITE setTimelineIndentWidth NOTIFY timelineIndentWidthChanged FINAL)

    muse::Inject<ISpectrogramPainter> spectrogramPainter;
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    SpectrogramView(QQuickItem* parent = nullptr);
    ~SpectrogramView() override = default;

    int timelineIndentWidth() const { return m_timelineIndentWidth; }
    void setTimelineIndentWidth(int width);

signals:
    void timelineIndentWidthChanged();

private:
    void setTimelineContext(TimelineContext* newContext) override;
    void paint(QPainter* painter) override;

    int m_timelineIndentWidth = 0;
};
}
