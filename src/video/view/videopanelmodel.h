/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOPANELMODEL_H
#define AU_VIDEO_VIDEOPANELMODEL_H

#include <QObject>
#include <QString>

#include "global/async/asyncable.h"
#include "framework/interactive/iinteractive.h"
#include "modularity/ioc.h"

#include "../ivideoservice.h"

namespace au::video {
//! Backs the video panel's chrome: the attach and detach controls, and the
//! line of text that says what is going on when there is no picture.
class VideoPanelModel : public QObject, public muse::async::Asyncable,
    public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(bool hasVideo READ hasVideo NOTIFY stateChanged FINAL)
    Q_PROPERTY(QString statusText READ statusText NOTIFY stateChanged FINAL)
    Q_PROPERTY(QString sourceName READ sourceName NOTIFY stateChanged FINAL)

public:
    explicit VideoPanelModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void attachVideo();
    Q_INVOKABLE void detachVideo();

    bool hasVideo() const;
    QString statusText() const;
    QString sourceName() const;

signals:
    void stateChanged();

private:
    muse::ContextInject<IVideoService> videoService { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
};
}

#endif // AU_VIDEO_VIDEOPANELMODEL_H
