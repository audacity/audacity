/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <vector>

#include <QObject>
#include <QPixmap>
#include <QSize>

#include "framework/global/io/path.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/io/ifilesystem.h"
#include "au3wrap/iau3project.h"

class QPainter;

namespace au::project {
class ThumbnailLoader : public QObject, public muse::Contextable
{
    Q_OBJECT

    muse::GlobalInject<muse::io::IFileSystem> filesystem;
    muse::GlobalInject<au::au3::IAu3ProjectReader> au3ProjectReader;

    Q_PROPERTY(QString path READ path WRITE setPath NOTIFY pathChanged)
    Q_PROPERTY(QString placeholder READ placeholder WRITE setPlaceholder)

    Q_PROPERTY(QPixmap thumbnail READ thumbnail NOTIFY thumbnailChanged)

    Q_PROPERTY(int width READ width WRITE setWidth)
    Q_PROPERTY(int height READ height WRITE setHeight)

    Q_PROPERTY(QColor backgroundColor READ backgroundColor WRITE setBackgroundColor)
    Q_PROPERTY(QColor lineColor READ lineColor WRITE setLineColor)
    Q_PROPERTY(QColor borderColor READ borderColor WRITE setBorderColor)

public:
    ThumbnailLoader(QObject* parent = nullptr);

    QString path() const;
    void setPath(const QString& path);

    QString placeholder() const;
    void setPlaceholder(const QString& placeholder);

    QPixmap thumbnail() const;

    int width() const;
    void setWidth(int width);

    int height() const;
    void setHeight(int height);

    QColor backgroundColor() const;
    void setBackgroundColor(const QColor& color);

    QColor lineColor() const;
    void setLineColor(const QColor& color);

    QColor borderColor() const;
    void setBorderColor(const QColor& color);

signals:
    void pathChanged();
    void thumbnailChanged();

private:
    muse::io::path_t selectSource() const;

    void loadThumbnail();
    void setThumbnail(const QPixmap& thumbnail);

    std::vector<float> parseWaveformData(const muse::io::path_t& source) const;
    void drawWaveform(QPainter& painter, const std::vector<float>& points) const;

    QPixmap renderFromProject(const muse::io::path_t& source);
    QPixmap renderWaveformToPixmap(const muse::io::path_t& source);
    QPixmap renderFromImage(const muse::io::path_t& source);
    void drawBorder(QPainter& painter) const;

    muse::io::path_t m_path;
    muse::io::path_t m_placeholder;

    int m_width = 0;
    int m_height = 0;

    QPixmap m_thumbnail;

    QColor m_backgroundColor;
    QColor m_lineColor;
    QColor m_borderColor;
};
}
