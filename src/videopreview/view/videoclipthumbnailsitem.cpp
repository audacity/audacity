/*
* Audacity: A Digital Audio Editor
*/
#include "videoclipthumbnailsitem.h"

#include <algorithm>
#include <cmath>
#include <optional>
#include <thread>

#include <QFileInfo>
#include <QMetaObject>
#include <QPainter>
#include <QPointer>
#include <QVariant>

#include "../internal/ffmpegvideodecoder.h"

using namespace au::videopreview;

namespace {
constexpr double EPS = 1e-7;
constexpr double THUMBNAIL_INTERVAL_SECONDS = 5.0;

double duration(double start, double end)
{
    return std::max(0.0, end - start);
}

double sourceAtProjectTime(const VideoSegment& segment, double projectTime)
{
    const double projectDuration = duration(segment.projectStart, segment.projectEnd);
    const double sourceDuration = duration(segment.sourceStart, segment.sourceEnd);
    if (projectDuration <= EPS || sourceDuration <= EPS) {
        return segment.sourceStart;
    }

    const double normalized = std::clamp((projectTime - segment.projectStart) / projectDuration, 0.0, 1.0);
    return segment.sourceStart + normalized * sourceDuration;
}

std::optional<std::pair<VideoLink, VideoSegment> > findSegment(const VideoLinks& links, int64_t trackId, int64_t itemId)
{
    for (const VideoLink& link : links) {
        if (link.trackId != trackId) {
            continue;
        }

        for (const VideoSegment& segment : link.segments) {
            if (segment.clipKey.itemId == itemId) {
                return std::make_pair(link, segment);
            }
        }
    }

    return std::nullopt;
}
}

VideoClipThumbnailsItem::VideoClipThumbnailsItem(QQuickItem* parent)
    : QQuickPaintedItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setAntialiasing(false);
}

VideoClipThumbnailsItem::~VideoClipThumbnailsItem()
{
    ++m_generation;
}

void VideoClipThumbnailsItem::init()
{
    if (m_inited || !service()) {
        return;
    }

    service()->linkChanged().onNotify(this, [this]() {
        scheduleReload();
    });

    m_inited = true;
    scheduleReload();
}

QVariant VideoClipThumbnailsItem::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void VideoClipThumbnailsItem::setTrackId(const QVariant& trackId)
{
    const int64_t value = trackId.toLongLong();
    if (m_trackId == value) {
        return;
    }

    m_trackId = value;
    emit clipChanged();
    scheduleReload();
}

QVariant VideoClipThumbnailsItem::itemId() const
{
    return QVariant::fromValue(m_itemId);
}

void VideoClipThumbnailsItem::setItemId(const QVariant& itemId)
{
    const int64_t value = itemId.toLongLong();
    if (m_itemId == value) {
        return;
    }

    m_itemId = value;
    emit clipChanged();
    scheduleReload();
}

double VideoClipThumbnailsItem::projectStart() const
{
    return m_projectStart;
}

void VideoClipThumbnailsItem::setProjectStart(double projectStart)
{
    if (std::abs(m_projectStart - projectStart) <= EPS) {
        return;
    }

    m_projectStart = projectStart;
    emit clipChanged();
    scheduleReload();
}

double VideoClipThumbnailsItem::projectEnd() const
{
    return m_projectEnd;
}

void VideoClipThumbnailsItem::setProjectEnd(double projectEnd)
{
    if (std::abs(m_projectEnd - projectEnd) <= EPS) {
        return;
    }

    m_projectEnd = projectEnd;
    emit clipChanged();
    scheduleReload();
}

void VideoClipThumbnailsItem::scheduleReload()
{
    if (!m_inited || !service()) {
        return;
    }

    const uint64_t generation = ++m_generation;
    QMetaObject::invokeMethod(this, [this, generation]() {
        if (generation == m_generation) {
            reload();
        }
    }, Qt::QueuedConnection);
}

void VideoClipThumbnailsItem::reload()
{
    if (!service() || m_trackId == -1 || m_itemId == -1 || duration(m_projectStart, m_projectEnd) <= EPS) {
        setThumbnails(++m_generation, {});
        return;
    }

    const VideoLinks links = service()->links();
    const auto match = findSegment(links, m_trackId, m_itemId);
    if (!match.has_value() || !QFileInfo::exists(match->first.sourcePath.toQString())) {
        setThumbnails(++m_generation, {});
        return;
    }

    const uint64_t generation = m_generation.load();
    const VideoLink link = match->first;
    const VideoSegment segment = match->second;
    const double projectStart = std::max(m_projectStart, segment.projectStart);
    const double projectEnd = std::min(m_projectEnd, segment.projectEnd);
    QPointer<VideoClipThumbnailsItem> self(this);

    std::thread([self, generation, link, segment, projectStart, projectEnd]() {
        std::vector<Thumbnail> thumbnails;
        for (double projectTime = projectStart; projectTime < projectEnd - EPS; projectTime += THUMBNAIL_INTERVAL_SECONDS) {
            const double tileEnd = std::min(projectTime + THUMBNAIL_INTERVAL_SECONDS, projectEnd);
            VideoDecodeResult result = FFmpegVideoDecoder::decodeFrame(
                link.sourcePath,
                link.streamIndex,
                link.streamId,
                sourceAtProjectTime(segment, projectTime));

            if (result.isUsable()) {
                thumbnails.push_back(Thumbnail {
                    projectTime,
                    tileEnd,
                    std::move(result.frame)
                });
            }
        }

        if (!self) {
            return;
        }

        QMetaObject::invokeMethod(self, [self, generation, thumbnails = std::move(thumbnails)]() mutable {
            if (self) {
                self->setThumbnails(generation, std::move(thumbnails));
            }
        }, Qt::QueuedConnection);
    }).detach();
}

void VideoClipThumbnailsItem::setThumbnails(uint64_t generation, std::vector<Thumbnail> thumbnails)
{
    if (generation != m_generation) {
        return;
    }

    {
        std::scoped_lock lock(m_mutex);
        m_thumbnails = std::move(thumbnails);
    }

    update();
}

void VideoClipThumbnailsItem::paint(QPainter* painter)
{
    painter->fillRect(boundingRect(), QColor(28, 28, 28));

    std::vector<Thumbnail> thumbnails;
    {
        std::scoped_lock lock(m_mutex);
        thumbnails = m_thumbnails;
    }

    const double clipDuration = duration(m_projectStart, m_projectEnd);
    if (clipDuration <= EPS || thumbnails.empty()) {
        painter->fillRect(boundingRect(), QColor(245, 245, 245, 32));
        return;
    }

    painter->setRenderHint(QPainter::SmoothPixmapTransform, true);
    for (const Thumbnail& thumbnail : thumbnails) {
        if (thumbnail.frame.isNull()) {
            continue;
        }

        const double left = (thumbnail.projectStart - m_projectStart) / clipDuration * boundingRect().width();
        const double right = (thumbnail.projectEnd - m_projectStart) / clipDuration * boundingRect().width();
        const QRectF tileRect(left, 0.0, std::max(1.0, right - left), boundingRect().height());
        painter->drawImage(tileRect, thumbnail.frame);
        painter->fillRect(QRectF(tileRect.right() - 1.0, 0.0, 1.0, tileRect.height()), QColor(0, 0, 0, 80));
    }
}
