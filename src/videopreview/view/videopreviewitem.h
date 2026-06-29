/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QQuickPaintedItem>

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "../ivideopreviewservice.h"

namespace au::videopreview {
class VideoPreviewItem : public QQuickPaintedItem, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::ContextInject<IVideoPreviewService> service{ this };

public:
    explicit VideoPreviewItem(QQuickItem* parent = nullptr);

    Q_INVOKABLE void init();

    void paint(QPainter* painter) override;

private:
    bool m_inited = false;
};
}
