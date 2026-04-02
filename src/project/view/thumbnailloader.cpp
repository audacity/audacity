/*
* Audacity: A Digital Audio Editor
*/
#include "thumbnailloader.h"

#include <QPixmap>
#include <qnamespace.h>

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

QPixmap ThumbnailLoader::renderWaveformToPixmap()
{
    if (m_width <= 0 || m_height <= 0) {
        return QPixmap();
    }

    muse::RetVal<muse::ByteArray> result = filesystem()->readFile(m_path);
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
    if (points.empty() || m_width <= 0 || m_height <= 0) {
        return QPixmap();
    }

    QPixmap pixmap(QSize(m_width, m_height));
    pixmap.fill(m_backgroundColor);

    QPainter painter(&pixmap);
    if (!painter.isActive()) {
        return QPixmap();
    }
    painter.setRenderHint(QPainter::Antialiasing, true);

    painter.setPen(Qt::NoPen);
    painter.setBrush(m_lineColor);

    const double midY = m_height / 2.0;
    const double barWidth = m_width / static_cast<double>(points.size());

    for (unsigned i = 0; i < points.size(); ++i) {
        const double barHeight = qMax(2.0, static_cast<double>(points[i]) * m_height * 0.9);
        painter.drawRect(QRectF(
                             i * barWidth,
                             midY - barHeight / 2.0,
                             qMax(1.0, barWidth - 0.5),
                             barHeight
                             ));
    }

    if (m_borderColor.isValid() && m_borderColor.alpha() > 0) {
        painter.setPen(QPen(m_borderColor, 1));
        painter.setBrush(Qt::NoBrush);
        painter.drawRect(QRectF(0, 0, m_width, m_height).adjusted(0.5, 0.5, -0.5, -0.5));
    }

    return pixmap;
}

QPixmap ThumbnailLoader::renderFromImage()
{
    if (m_width <= 0 || m_height <= 0) {
        return QPixmap();
    }

    const bool isPlaceholder = !filesystem()->exists(m_path);

    const QString pixmapPath = isPlaceholder ? QString(DEFAULT_PLACEHOLDER) : m_path;
    QPixmap pixmap(pixmapPath);
    if (pixmap.isNull()) {
        return QPixmap();
    }

    const QSize targetSize = isPlaceholder ? QSize(m_width / 2, m_height / 2) : QSize(m_width, m_height);
    QPixmap scaledPixmap = pixmap.scaled(targetSize, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);

    QPixmap finalPixmap(QSize(m_width, m_height));
    finalPixmap.fill(Qt::transparent);

    QPainter painter(&finalPixmap);
    if (!painter.isActive()) {
        return QPixmap();
    }
    painter.setRenderHint(QPainter::Antialiasing, true);

    painter.fillRect(QRectF(0, 0, m_width, m_height), m_backgroundColor);

    const QPointF center((m_width - scaledPixmap.width()) / 2.0, (m_height - scaledPixmap.height()) / 2.0);
    painter.drawPixmap(center, scaledPixmap);

    if (m_borderColor.isValid() && m_borderColor.alpha() > 0) {
        painter.setPen(QPen(m_borderColor, 1));
        painter.setBrush(Qt::NoBrush);
        painter.drawRect(QRectF(0, 0, m_width, m_height).adjusted(0.5, 0.5, -0.5, -0.5));
    }

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

    if (m_width > 0 && m_height > 0) {
        loadThumbnail();
    }
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

    if (m_width > 0 && m_height > 0) {
        loadThumbnail();
    }
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

    loadThumbnail();
}

QColor ThumbnailLoader::lineColor() const
{
    return m_lineColor;
}

void ThumbnailLoader::setLineColor(const QColor& color)
{
    if (m_lineColor == color) {
        return;
    }

    m_lineColor = color;

    loadThumbnail();
}

QColor ThumbnailLoader::borderColor() const
{
    return m_borderColor;
}

void ThumbnailLoader::setBorderColor(const QColor& color)
{
    if (m_borderColor == color) {
        return;
    }

    m_borderColor = color;

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
                              ? renderWaveformToPixmap()
                              : renderFromImage();

    setThumbnail(thumbnail);
}

void ThumbnailLoader::setThumbnail(const QPixmap& thumbnail)
{
    if (m_thumbnail.cacheKey() == thumbnail.cacheKey()) {
        return;
    }

    m_thumbnail = thumbnail;
    emit thumbnailChanged();
}
