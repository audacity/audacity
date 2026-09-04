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

    //! The reopened file does not match what was saved with the project.
    Q_PROPERTY(bool sourceMismatch READ sourceMismatch NOTIFY stateChanged FINAL)
    Q_PROPERTY(QString warningText READ warningText NOTIFY stateChanged FINAL)

public:
    explicit VideoPanelModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void attachVideo();
    Q_INVOKABLE void detachVideo();

    //! Opens the preferences page carrying the FFmpeg download and locate
    //! controls. "FFmpeg not found" is otherwise a dead end: Audacity does
    //! not ship it, and that page is the only working pointer in the app.
    Q_INVOKABLE void openFFmpegPreferences();

    //! Whether the panel should offer that route.
    Q_PROPERTY(bool needsFFmpeg READ needsFFmpeg NOTIFY stateChanged FINAL)

    //! Seconds the picture is shifted along the timeline; see IVideoService.
    Q_PROPERTY(double offset READ offset WRITE setOffset NOTIFY offsetChanged FINAL)

    //! The offset formatted for display, e.g. "+1.250 s". Empty at zero.
    Q_PROPERTY(QString offsetText READ offsetText NOTIFY offsetChanged FINAL)

    bool hasVideo() const;
    QString statusText() const;
    QString sourceName() const;
    bool sourceMismatch() const;
    bool needsFFmpeg() const;

    double offset() const;
    void setOffset(double offset);
    QString offsetText() const;
    QString warningText() const;

signals:
    void stateChanged();
    void offsetChanged();

private:
    muse::ContextInject<IVideoService> videoService { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
};
}

#endif // AU_VIDEO_VIDEOPANELMODEL_H
