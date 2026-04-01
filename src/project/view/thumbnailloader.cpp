/*
* Audacity: A Digital Audio Editor
*/
#include "thumbnailloader.h"

#include <QFileInfo>
#include <QUrl>

using namespace au::project;

namespace {
QPixmap renderWaveformToPixmap(const QVector<float>& points, const QSize& size, const QColor& backgroundColor)
{
    if (points.isEmpty() || size.isEmpty()) {
        return QPixmap();
    }

    QPixmap pixmap(size);
    pixmap.fill(Qt::transparent);

    QPainter painter(&pixmap);
    painter.setRenderHint(QPainter::Antialiasing, true);

    painter.fillRect(QRectF(0, 0, size.width(), size.height()), backgroundColor);

    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor(0xE4, 0xE4, 0xE4));

    const double midY = size.height() / 2.0;
    const double barWidth = size.width() / static_cast<double>(points.size());

    for (int i = 0; i < points.size(); ++i) {
        const double barHeight = qMax(2.0, static_cast<double>(points[i]) * size.height() * 0.9);
        painter.drawRect(QRectF(
                             i * barWidth,
                             midY - barHeight / 2.0,
                             qMax(1.0, barWidth - 0.5),
                             barHeight
                             ));
    }

    return pixmap;
}
}

ThumbnailLoader::ThumbnailLoader(QObject* parent)
    : QObject(parent)
{
}

QString ThumbnailLoader::path() const
{
    return m_path;
}

void ThumbnailLoader::setPath(const QString& path)
{
    if (m_path == path) {
        return;
    }

    m_path = path;
    emit pathChanged();

    loadThumbnail();
}

QSize ThumbnailLoader::thumbnailSize() const
{
    return m_thumbnailSize;
}

void ThumbnailLoader::setThumbnailSize(const QSize& size)
{
    if (m_thumbnailSize == size) {
        return;
    }

    m_thumbnailSize = size;
    emit thumbnailSizeChanged();

    loadThumbnail();
}

QColor ThumbnailLoader::backgroundColor() const
{
    return m_backgroundColor;
}

void ThumbnailLoader::setBackgroundColor(const QColor& color)
{
    if (m_backgroundColor == color) {
        return;
    }

    m_backgroundColor = color;
    emit backgroundColorChanged();

    loadThumbnail();
}

bool ThumbnailLoader::isThumbnailValid() const
{
    return !m_thumbnail.isNull();
}

QPixmap ThumbnailLoader::thumbnail() const
{
    return m_thumbnail;
}

void ThumbnailLoader::loadThumbnail()
{
    if (m_path.isEmpty()) {
        setThumbnail(QPixmap());
        return;
    }

    const QString localPath = QUrl(path()).toLocalFile();
    QFile file(localPath.isEmpty() ? path() : localPath);
    if (!file.open(QIODevice::ReadOnly)) {
        setThumbnail(QPixmap());
        return;
    }

    const QJsonDocument doc = QJsonDocument::fromJson(file.readAll());
    if (!doc.isArray()) {
        setThumbnail(QPixmap());
        return;
    }

    const QJsonArray arr = doc.array();
    QVector<float> points;
    points.reserve(arr.size());
    for (const QJsonValue& v : arr) {
        points.append(static_cast<float>(qBound(0.0, v.toDouble(), 1.0)));
    }

    setThumbnail(renderWaveformToPixmap(points, m_thumbnailSize, m_backgroundColor));
}

void ThumbnailLoader::setThumbnail(const QPixmap& thumbnail)
{
    m_thumbnail = thumbnail;
    emit thumbnailChanged();
}
