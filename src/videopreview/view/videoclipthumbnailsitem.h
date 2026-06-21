/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <mutex>
#include <vector>

#include <QImage>
#include <QQuickPaintedItem>

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "../ivideopreviewservice.h"

namespace au::videopreview {
class VideoClipThumbnailsItem : public QQuickPaintedItem, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY clipChanged FINAL)
    Q_PROPERTY(QVariant itemId READ itemId WRITE setItemId NOTIFY clipChanged FINAL)
    Q_PROPERTY(double projectStart READ projectStart WRITE setProjectStart NOTIFY clipChanged FINAL)
    Q_PROPERTY(double projectEnd READ projectEnd WRITE setProjectEnd NOTIFY clipChanged FINAL)

    muse::ContextInject<IVideoPreviewService> service{ this };

public:
    explicit VideoClipThumbnailsItem(QQuickItem* parent = nullptr);
    ~VideoClipThumbnailsItem() override;

    Q_INVOKABLE void init();

    QVariant trackId() const;
    void setTrackId(const QVariant& trackId);

    QVariant itemId() const;
    void setItemId(const QVariant& itemId);

    double projectStart() const;
    void setProjectStart(double projectStart);

    double projectEnd() const;
    void setProjectEnd(double projectEnd);

    void paint(QPainter* painter) override;

signals:
    void clipChanged();

private:
    struct Thumbnail
    {
        double projectStart = 0.0;
        double projectEnd = 0.0;
        QImage frame;
    };

    void scheduleReload();
    void reload();
    void setThumbnails(uint64_t generation, std::vector<Thumbnail> thumbnails);

    int64_t m_trackId = -1;
    int64_t m_itemId = -1;
    double m_projectStart = 0.0;
    double m_projectEnd = 0.0;
    bool m_inited = false;
    std::atomic<uint64_t> m_generation { 0 };

    mutable std::mutex m_mutex;
    std::vector<Thumbnail> m_thumbnails;
};
}
