/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogrampainter.h"
#include "abstractclipview.h"

#include "trackedit/iselectioncontroller.h"

namespace au::projectscene {
class SpectrogramView : public AbstractClipView
{
    Q_OBJECT

    muse::Inject<ISpectrogramPainter> spectrogramPainter;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    SpectrogramView(QQuickItem* parent = nullptr);
    ~SpectrogramView() override = default;

    void setTimelineContext(TimelineContext* newContext) override;

    void paint(QPainter* painter) override;
};
}
