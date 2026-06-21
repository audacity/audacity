/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "async/asyncable.h"
#include "modularity/ioc.h"

#include "../ivideopreviewservice.h"

namespace au::videopreview {

class VideoPreviewModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(QString stateText READ stateText NOTIFY stateChanged)
    Q_PROPERTY(bool hasFrame READ hasFrame NOTIFY frameChanged)
    Q_PROPERTY(QString sourcePath READ sourcePath NOTIFY linkChanged)
    Q_PROPERTY(double aspectRatio READ aspectRatio NOTIFY aspectRatioChanged)
    Q_PROPERTY(bool available READ available NOTIFY stateChanged)

    muse::ContextInject<IVideoPreviewService> service{ this };

public:
    explicit VideoPreviewModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    QString stateText() const;
    bool hasFrame() const;
    QString sourcePath() const;
    double aspectRatio() const;
    bool available() const;

signals:
    void stateChanged();
    void frameChanged();
    void linkChanged();
    void aspectRatioChanged();

private:
    bool m_inited = false;
};
}
