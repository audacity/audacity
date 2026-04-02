/*
* Audacity: A Digital Audio Editor
*/
#include "thumbnailloader.h"

#include <QPixmap>

#include "framework/global/io/path.h"
#include "modularity/ioc.h"

using namespace au::project;

namespace {
constexpr auto DEFAULT_PLACEHOLDER = ":/resources/ProjectPlaceholder.svg";
}

ThumbnailLoader::ThumbnailLoader(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this)), m_placeholder(DEFAULT_PLACEHOLDER)
{
}

QPixmap ThumbnailLoader::renderWaveformToPixmap(const muse::io::path_t& path, const QSize& size, const QColor& backgroundColor)
{
    if (size.isEmpty()) {
        return QPixmap();
    }

    muse::RetVal<muse::ByteArray> result = filesystem()->readFile(path);
    if (!result.ret) {
        return QPixmap();
    }

    const QJsonDocument doc = QJsonDocument::fromJson(result.val.toQByteArray());
    if (!doc.isArray()) {
        return QPixmap();
    }

    const QJsonArray arr = doc.array();

    std::vector<float> points;
    points.reserve(arr.size());

    for (const auto& v : arr) {
        points.push_back(static_cast<float>(qBound(0.0, v.toDouble(), 1.0)));
    }
    if (points.empty() || size.isEmpty()) {
        return QPixmap();
    }

    QPixmap pixmap(size);
    pixmap.fill(Qt::transparent);

    QPainter painter(&pixmap);
    if (!painter.isActive()) {
        return QPixmap();
    }
    painter.setRenderHint(QPainter::Antialiasing, true);

    painter.fillRect(QRectF(0, 0, size.width(), size.height()), backgroundColor);

    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor(0xE4, 0xE4, 0xE4));

    const double midY = size.height() / 2.0;
    const double barWidth = size.width() / static_cast<double>(points.size());

    for (unsigned i = 0; i < points.size(); ++i) {
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

QPixmap ThumbnailLoader::renderFromImage(const muse::io::path_t& path, const QSize& size, const QColor& backgroundColor)
{
    if (size.isEmpty()) {
        return QPixmap();
    }

    const bool isPlaceholder = !filesystem()->exists(path);

    const QString pixmapPath = isPlaceholder ? QString(DEFAULT_PLACEHOLDER) : path.toQString();
    QPixmap pixmap(pixmapPath);
    if (pixmap.isNull()) {
        return QPixmap();
    }

    const QSize targetSize = isPlaceholder ? size / 2 : size;
    QPixmap scaledPixmap = pixmap.scaled(targetSize, Qt::KeepAspectRatio, Qt::SmoothTransformation);

    QPixmap finalPixmap(size);
    finalPixmap.fill(Qt::transparent);

    QPainter painter(&finalPixmap);
    if (!painter.isActive()) {
        return QPixmap();
    }
    painter.setRenderHint(QPainter::Antialiasing, true);

    painter.fillRect(QRectF(0, 0, size.width(), size.height()), backgroundColor);

    const QPointF center((size.width() - scaledPixmap.width()) / 2.0, (size.height() - scaledPixmap.height()) / 2.0);
    painter.drawPixmap(center, scaledPixmap);

    return finalPixmap;
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

QString ThumbnailLoader::placeholder() const
{
    return m_placeholder;
}

void ThumbnailLoader::setPlaceholder(const QString& placeholder)
{
    if (m_placeholder == placeholder) {
        return;
    }

    m_placeholder = placeholder.isEmpty() ? DEFAULT_PLACEHOLDER : placeholder;

    if (m_path.isEmpty()) {
        loadThumbnail();
    }
}

int ThumbnailLoader::width() const
{
    return m_width;
}

void ThumbnailLoader::setWidth(int width)
{
    if (m_width == width) {
        return;
    }

    m_width = width;

    loadThumbnail();
}

int ThumbnailLoader::height() const
{
    return m_height;
}

void ThumbnailLoader::setHeight(int height)
{
    if (m_height == height) {
        return;
    }

    m_height = height;

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

    const muse::io::path_t filePath(m_path);
    const QSize size(m_width, m_height);
    const QPixmap thumbnail = filePath.hasSuffix("json")
                              ? renderWaveformToPixmap(filePath, size, m_backgroundColor)
                              : renderFromImage(filePath, size, m_backgroundColor);

    setThumbnail(thumbnail);
}

void ThumbnailLoader::setThumbnail(const QPixmap& thumbnail)
{
    m_thumbnail = thumbnail;
    emit thumbnailChanged();
}
