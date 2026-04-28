/*
* Audacity: A Digital Audio Editor
*/
#include "thumbnailloader.h"

#include <QJsonArray>
#include <QJsonDocument>
#include <QPainter>
#include <QPen>
#include <QPixmap>
#include <qpixmap.h>

using namespace au::project;

namespace {
const muse::io::path_t DEFAULT_PLACEHOLDER(":/resources/ProjectPlaceholder.svg");
}

ThumbnailLoader::ThumbnailLoader(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this)), m_placeholder(DEFAULT_PLACEHOLDER)
{
}

muse::io::path_t ThumbnailLoader::selectSource() const
{
    if (!m_path.empty() && filesystem()->exists(m_path)) {
        return m_path;
    }

    if (!m_placeholder.empty() && filesystem()->exists(m_placeholder)) {
        return m_placeholder;
    }

    return DEFAULT_PLACEHOLDER;
}

void ThumbnailLoader::drawBorder(QPainter& painter) const
{
    if (!m_borderColor.isValid() || m_borderColor.alpha() == 0) {
        return;
    }
    painter.setPen(QPen(m_borderColor, 1));
    painter.setBrush(Qt::NoBrush);
    painter.drawRect(QRectF(0, 0, m_width, m_height).adjusted(0.5, 0.5, -0.5, -0.5));
}

std::vector<float> ThumbnailLoader::parseWaveformData(const muse::io::path_t& source) const
{
    muse::RetVal<muse::ByteArray> result = filesystem()->readFile(source);
    if (!result.ret) {
        return {};
    }

    const QJsonDocument doc = QJsonDocument::fromJson(result.val.toQByteArray());
    if (!doc.isArray()) {
        return {};
    }

    const QJsonArray arr = doc.array();
    std::vector<float> points;
    points.reserve(arr.size());
    for (const auto& v : arr) {
        points.push_back(static_cast<float>(qBound(0.0, v.toDouble(), 1.0)));
    }
    return points;
}

void ThumbnailLoader::drawWaveform(QPainter& painter, const std::vector<float>& points) const
{
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
}

QPixmap ThumbnailLoader::renderFromProject(const muse::io::path_t& source)
{
    if (m_width <= 0 || m_height <= 0) {
        return QPixmap();
    }

    const auto pngData = au3ProjectReader()->readProjectThumbnail(source);

    if (!pngData.has_value()) {
        return QPixmap();
    }

    QPixmap pixmap;
    if (!pixmap.loadFromData(pngData->data(), static_cast<uint>(pngData->size()), "PNG")) {
        LOGE() << "Failed to decode thumbnail for: " << source.toQString();
        return QPixmap();
    }

    return pixmap;
}

QPixmap ThumbnailLoader::renderWaveformToPixmap(const muse::io::path_t& source)
{
    if (m_width <= 0 || m_height <= 0) {
        return QPixmap();
    }

    const std::vector<float> points = parseWaveformData(source);
    if (points.empty()) {
        return QPixmap();
    }

    QPixmap pixmap(QSize(m_width, m_height));
    pixmap.fill(m_backgroundColor);

    QPainter painter(&pixmap);
    if (!painter.isActive()) {
        return QPixmap();
    }
    painter.setRenderHint(QPainter::Antialiasing, true);

    drawWaveform(painter, points);
    drawBorder(painter);

    return pixmap;
}

QPixmap ThumbnailLoader::renderFromImage(const muse::io::path_t& source)
{
    if (m_width <= 0 || m_height <= 0) {
        return QPixmap();
    }

    QPixmap pixmap(source.toQString());
    if (pixmap.isNull()) {
        return QPixmap();
    }

    const bool isDefault = (source == m_placeholder) || (source == DEFAULT_PLACEHOLDER);
    const auto aspectRatio = isDefault ? Qt::KeepAspectRatio : Qt::IgnoreAspectRatio;
    const QSize targetSize = isDefault ? QSize(m_width / 2, m_height / 2) : QSize(m_width, m_height);
    const QPixmap scaledPixmap = pixmap.scaled(targetSize, aspectRatio, Qt::SmoothTransformation);

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

    drawBorder(painter);

    return finalPixmap;
}

QString ThumbnailLoader::path() const
{
    return m_path.toQString();
}

void ThumbnailLoader::setPath(const QString& path)
{
    const muse::io::path_t newPath(path);
    if (m_path == newPath) {
        return;
    }

    m_path = newPath;
    emit pathChanged();

    loadThumbnail();
}

QString ThumbnailLoader::placeholder() const
{
    return m_placeholder.toQString();
}

void ThumbnailLoader::setPlaceholder(const QString& placeholder)
{
    const muse::io::path_t newPlaceholder = placeholder.isEmpty() ? DEFAULT_PLACEHOLDER : muse::io::path_t(placeholder);
    if (m_placeholder == newPlaceholder) {
        return;
    }

    m_placeholder = newPlaceholder;

    if (m_path.empty()) {
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
    if (m_width <= 0 || m_height <= 0) {
        return;
    }

    const muse::io::path_t source = selectSource();

    QPixmap thumbnail {};
    if (source.hasSuffix("aup4")) {
        thumbnail = renderFromProject(source);
    } else if (source.hasSuffix("json")) {
        thumbnail = renderWaveformToPixmap(source);
    } else {
        thumbnail = renderFromImage(source);
    }

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
