/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/ispectrogrampainter.h"
#include "abstractclipview.h"

#include "context/iglobalcontext.h"

#include "framework/global/async/asyncable.h"

namespace au::projectscene {
class SpectrogramView : public AbstractClipView, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int timelineIndentWidth READ timelineIndentWidth WRITE setTimelineIndentWidth NOTIFY timelineIndentWidthChanged FINAL)

    muse::Inject<spectrogram::ISpectrogramPainter> spectrogramPainter;
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
    void classBegin() override {}
    void componentComplete() override;

    int m_timelineIndentWidth = 0;
};
}
