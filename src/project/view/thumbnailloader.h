/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QFileSystemWatcher>
#include <QObject>
#include <QPixmap>
#include <QSize>

namespace au::project {
class ThumbnailLoader : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString path READ path WRITE setPath NOTIFY pathChanged)

    Q_PROPERTY(bool isThumbnailValid READ isThumbnailValid NOTIFY thumbnailChanged)
    Q_PROPERTY(QPixmap thumbnail READ thumbnail NOTIFY thumbnailChanged)

    Q_PROPERTY(QSize thumbnailSize READ thumbnailSize WRITE setThumbnailSize NOTIFY thumbnailSizeChanged)
    Q_PROPERTY(QColor backgroundColor READ backgroundColor WRITE setBackgroundColor NOTIFY backgroundColorChanged)

public:
    ThumbnailLoader(QObject* parent = nullptr);

    bool isThumbnailValid() const;
    QPixmap thumbnail() const;

    QString path() const;
    void setPath(const QString& path);

    QSize thumbnailSize() const;
    void setThumbnailSize(const QSize& size);

    QColor backgroundColor() const;
    void setBackgroundColor(const QColor& color);

signals:
    void pathChanged();
    void thumbnailChanged();
    void thumbnailSizeChanged();
    void backgroundColorChanged();

private:
    void loadThumbnail();
    void setThumbnail(const QPixmap& thumbnail);

    QPixmap m_thumbnail;
    QString m_path;
    QSize m_thumbnailSize;
    QColor m_backgroundColor;
};
}
