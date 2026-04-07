#pragma once

#include <QObject>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "au3wrap/iau3project.h"

namespace au::project {
class ProjectThumbnailLoader : public QObject, public muse::async::Asyncable
{
    Q_OBJECT;

    muse::GlobalInject<au::au3::IAu3ProjectReader> au3ProjectReader;

    Q_PROPERTY(QString projectPath READ projectPath WRITE setProjectPath NOTIFY projectPathChanged)

    Q_PROPERTY(bool isThumbnailValid READ isThumbnailValid NOTIFY thumbnailChanged)
    Q_PROPERTY(QPixmap thumbnail READ thumbnail NOTIFY thumbnailChanged)

public:
    ProjectThumbnailLoader(QObject* parent = nullptr);

    bool isThumbnailValid() const;
    QPixmap thumbnail() const;

    QString projectPath() const;
    void setProjectPath(const QString& projectPath);

signals:
    void thumbnailChanged();

    void projectPathChanged();

private:
    void loadThumbnail();
    void setThumbnail(const QPixmap& thumbnail);

    QPixmap m_thumbnail;
    QString m_projectPath;
};
}
